graph TD;
  v62["examples/c-rpm/rpm/include/rpm/argv.h"]
  v59["examples/c-rpm/rpm/include/rpm/header.h"]
  v64["examples/c-rpm/rpm/include/rpm/rpmds.h"]
  v2["examples/c-rpm/rpm/include/rpm/rpmio.h"]
  v0["examples/c-rpm/rpm/include/rpm/rpmlib.h"]
  v73["examples/c-rpm/rpm/include/rpm/rpmprob.h"]
  v72["examples/c-rpm/rpm/include/rpm/rpmps.h"]
  v52["examples/c-rpm/rpm/include/rpm/rpmsw.h"]
  v61["examples/c-rpm/rpm/include/rpm/rpmtag.h"]
  v60["examples/c-rpm/rpm/include/rpm/rpmtd.h"]
  v46["examples/c-rpm/rpm/include/rpm/rpmtypes.h"]
  v63["examples/c-rpm/rpm/include/rpm/rpmutil.h"]
  v74["examples/c-rpm/rpm/include/rpm/rpmver.h"]
  v0-->|"#include &lt;rpm/rpmio.h&gt;"|v2
  v59-->|"#include &lt;rpm/rpmio.h&gt;"|v2
  v2-->|"#include &lt;rpm/rpmtypes.h&gt;"|v46
  v59-->|"#include &lt;rpm/rpmtypes.h&gt;"|v46
  v61-->|"#include &lt;rpm/rpmtypes.h&gt;"|v46
  v62-->|"#include &lt;rpm/rpmtypes.h&gt;"|v46
  v64-->|"#include &lt;rpm/rpmtypes.h&gt;"|v46
  v72-->|"#include &lt;rpm/rpmtypes.h&gt;"|v46
  v73-->|"#include &lt;rpm/rpmtypes.h&gt;"|v46
  v74-->|"#include &lt;rpm/rpmtypes.h&gt;"|v46
  v2-->|"#include &lt;rpm/rpmsw.h&gt;"|v52
  v0-->|"#include &lt;rpm/header.h&gt;"|v59
  v59-->|"#include &lt;rpm/rpmtd.h&gt;"|v60
  v0-->|"#include &lt;rpm/rpmtag.h&gt;"|v61
  v60-->|"#include &lt;rpm/rpmtag.h&gt;"|v61
  v60-->|"#include &lt;rpm/argv.h&gt;"|v62
  v59-->|"#include &lt;rpm/rpmutil.h&gt;"|v63
  v64-->|"#include &lt;rpm/rpmutil.h&gt;"|v63
  v0-->|"#include &lt;rpm/rpmds.h&gt;"|v64
  v74-->|"#include &lt;rpm/rpmds.h&gt;"|v64
  v64-->|"#include &lt;rpm/rpmps.h&gt;"|v72
  v72-->|"#include &lt;rpm/rpmprob.h&gt;"|v73
  v0-->|"#include &lt;rpm/rpmver.h&gt;"|v74
