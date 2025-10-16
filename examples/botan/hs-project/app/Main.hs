{-# LANGUAGE OverloadedStrings #-}

-- | This code example is copied from the link below and adapted to
-- @hs-bindgen@-generated bindings.
--
-- <https://github.com/haskell-cryptography/botan/blob/486a71fd1142372e35534ddcbdc5165a73e46bb4/botan-low/test/Test/Botan/Low/SRP6/Example.hs>
module Main (main) where

{-
This is a simplified, executable example that should give you an idea of how to
use the functions in this module to run the SRP6 protocol. We will not explain
the protocol in detail here, so consult other sources if you need details about
the algorithm. The simplified example shows how a client and server run the
protocol from separate threads, keeping private values secret and communicating
public values using message passing. In practice, the client and server will
probably run on separate machines (or processes) with secure communication
channels in place, and other security measures are necessary. So, don't use this
example directly in your code if it runs in production.
-}

{-
Let us start with some necessary imports.
-}

import Control.Concurrent
import Control.Concurrent.Async
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Text.Printf

import Botan qualified

{-
The client and server exchange a number of messages during the protocol. They
will be explained in more detail later as they become relevant for the part of
the protocol that we are discussing. The messages will be exchanged from the
client to the server using an @MVar ClientMsg@, and from the server to the
client using an @MVar ServerMsg@.
-}

data ClientMsg =
    Signup Botan.Username Botan.Verifier
  | StartAuthentication Botan.Username
  | SendClientPublicKey Botan.A
data ServerMsg =
    SendServerPublicKey Botan.B

{-
SRP works in a discrete logarithm group, and it computes hashes using a hash
family. Both can be configured, and we will configure them as follows:
-}

group :: Botan.GroupId
group = Botan.MODP_SRP_4096
hash :: Botan.HashId
hash = Botan.SHA_512

{-
When a client wants to sign up with the server with a combination of an
identifier and password, the client first generates a verifier using the
identifier, the password, and a locally generated salt. Then, the client sends
their identifier and verifier to the server, while it stores its own salt
locally:
-}

clientSignup :: MVar ClientMsg -> Botan.Username -> Botan.Password -> IO Botan.Salt
clientSignup clientMsgVar identifier password =
    Botan.withRng (Botan.UserThreadsafe) $ \rng -> do
      salt <- Botan.Salt <$> Botan.rngGet rng 12
      verifier <- Botan.srp6GenerateVerifier identifier password salt group hash
      putMVar clientMsgVar $ Signup identifier verifier
      pure salt

{-
The server receives the signup request, and stores the identifier with the
verifier:
-}

type Users = Map Botan.Username Botan.Verifier
serverSignup :: MVar ClientMsg -> IORef Users -> IO ()
serverSignup clientMsgVar usersRef = do
    Signup identifier verifier <- takeMVar clientMsgVar
    modifyIORef usersRef $ Map.insert identifier verifier

{-
Later, the client may request to authenticate itself to the server:
-}

clientStartAuthentication :: MVar ClientMsg -> Botan.Username -> IO ()
clientStartAuthentication clientMsgVar identifier = do
    putMVar clientMsgVar $ StartAuthentication identifier

{-
The server receives the authentication request, looks up the verfier, generates
the server's public key (a SRP6 @B@ value), and sends it back to the client:
-}

serverStartAuthentication :: MVar ClientMsg -> MVar ServerMsg -> IORef Users -> Botan.ServerSession -> IO ()
serverStartAuthentication clientMsgVar serverMsg usersRef session = do
    StartAuthentication identifier <- takeMVar clientMsgVar
    users <- readIORef usersRef
    let verifier = fromJust $ Map.lookup identifier users
    Botan.withRng (Botan.UserThreadsafe) $ \rng -> do
      serverPublicKey <- Botan.srp6ServerSessionStep1 session verifier group hash rng
      putMVar serverMsg $ SendServerPublicKey serverPublicKey

{-
Once the client receives the server's public key, it generates the client's
public key (an SRP6 @B@ value) and the shared secret key, and sends the client's
public key to the server:
-}

clientAuthentication :: MVar ClientMsg -> MVar ServerMsg -> Botan.Username -> Botan.Password -> Botan.Salt -> IO Botan.K
clientAuthentication clientMsgVar serverMsgVar identifier password salt = do
    SendServerPublicKey serverPublicKey <- takeMVar serverMsgVar
    Botan.withRng (Botan.UserThreadsafe) $ \rng -> do
      (clientPublicKey, clientSecretKey) <-
        Botan.srp6ClientAgree identifier password group hash salt serverPublicKey rng
      putMVar clientMsgVar $ SendClientPublicKey clientPublicKey
      pure clientSecretKey

{-
The server then receives the client's public key, and generates a matching
shared secret key:
-}

serverAuthentication :: MVar ClientMsg -> Botan.ServerSession -> IO Botan.K
serverAuthentication clientMsgVar session = do
    SendClientPublicKey clientPublicKey <- takeMVar clientMsgVar
    serverSecretKey <- Botan.srp6ServerSessionStep2 session group clientPublicKey
    pure serverSecretKey

{-
At this point, the client's and servers' secret key should be equal. In
practice, the secret keys should be confirmed to match by exchanging a hash
digest of relevant protocol values, susch as the secret key, public keys,
identifier, salt, and more. Calculation of this hash digest is not part of this
example. Instead, we will return the secret keys from the client and server so
that we can compare them, which obviously breaks security, but this should be
fine for our example.

We bring the client-side of the algorithm together by picking an identifier and
password, and calling the client-side functions in order.
-}

client :: MVar ClientMsg -> MVar ServerMsg -> IO Botan.K
client clientMsgVar serverMsgVar = do
    let identifier = Botan.Username "alice"
        password = Botan.Password "Fee fi fo fum!"
    salt <- clientSignup clientMsgVar identifier password
    clientStartAuthentication clientMsgVar identifier
    clientAuthentication clientMsgVar serverMsgVar identifier password salt

{-
We bring the server-side of the algorithm together by initialising a session and
calling the server-side functions in order.
-}

server :: MVar ClientMsg -> MVar ServerMsg -> IO Botan.K
server clientMsgVar serverMsgVar = do
    usersRef <- newIORef Map.empty
    serverSignup clientMsgVar usersRef
    Botan.withServerSession $ \session -> do
      serverStartAuthentication clientMsgVar serverMsgVar usersRef session
      serverAuthentication clientMsgVar session

{-
Using the @async@ package, we spin up a thread for the client and a thread for
the server. We wait for them to finish, and then compare the resulting secret
keys.
-}

main :: IO ()
main = do
  clientMsgVar <- newEmptyMVar
  serverMsgVar <- newEmptyMVar
  (clientSecretKey, serverSecretKey) <-
    concurrently (client clientMsgVar serverMsgVar) (server clientMsgVar serverMsgVar)
  let match = clientSecretKey == serverSecretKey
  if match then
    print @String "client secret == server secret"
  else
    error $
      printf "%s /= %s"
              (show clientSecretKey)
              (show serverSecretKey)

{-
All that is left to do is run the program.
-}

