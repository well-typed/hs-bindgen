We have a two tiered CI. In particular

- We do not automatically run CI on pushes.
- We provide fast feedback on pull requests.
- We run extensive tests in the merge queue.

For this to work, the `haskell-ci.yml` workflow has to be amended manually. The
difference is

```diff
diff --git a/.github/workflows/haskell-ci.yml b/.github/workflows/haskell-ci.yml
index 7de2a924..f3a016c1 100644
--- a/.github/workflows/haskell-ci.yml
+++ b/.github/workflows/haskell-ci.yml
@@ -13,16 +13,19 @@
 # REGENDATA ("0.19.20250821",["github","cabal.project.ci"])
 #
 name: Haskell-CI
 on:
-  push:
-    branches:
-      - main
   pull_request:
     branches:
       - main
+  merge_group:
+    branches:
+      - main
+
+concurrency:
+  group: ${{ github.workflow }}-${{ github.event.pull_request.number || github.ref }}
+  cancel-in-progress: true
+
 jobs:
   linux:
     name: Haskell-CI - Linux - ${{ matrix.compiler }}
@@ -312,7 +315,7 @@ jobs:
           rm -f cabal.project.local
           $CABAL v2-build $ARG_COMPILER --disable-tests --disable-benchmarks all
       - name: save cache
-        if: always()
+        if: ${{ !cancelled() }}
         uses: actions/cache/save@v4
         with:
           key: ${{ runner.os }}-${{ matrix.compiler }}-${{ github.sha }}
```
