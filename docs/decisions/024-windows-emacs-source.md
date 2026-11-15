# 024 - One source-built Windows Emacs

Use an exact, unpatched upstream Emacs revision with native compilation/AOT and
the reviewed Flymake/TRAMP fixes, rather than maintaining local Lisp backports.
The builder owns its pinned source/bootstrap/options and isolated MSYS2 UCRT64
toolchain. Downloads are hash-verified; normal pacman signatures remain enabled.
Full toolchain updates are explicit and refused while source Emacs is running.

Keep generated sources, immutable builds, compiler, logs and reports under
ignored `.build\emacs\`. Reuse unchanged build inputs; atomically accept a new
candidate only after real native/AOT/async, required-feature and TLS tests pass.
Cache by source, options, toolchain inventory and explicit build-recipe version;
bump that version when compile/install semantics change. Orchestrator refactors
do not rebuild Emacs; changed tests revalidate the accepted binary. Original
build identity and immutable reports remain intact on revalidation. Failed
builds/checks preserve the previous acceptance.

The builder validates Emacs, not Doom. Existing setup owns Doom packages and
profiles; a separate post-setup smoke check covers Doom/Ghostel/Windows GUI.
Upstream Doctor diagnostics are not rewritten.

All managed Windows entry points use the accepted build and matching clients
through PowerShell 7.3+. Compiler/PATH settings are process-local. Launching does
not compare current pins with the receipt: editing build inputs must not prevent
connecting to an existing daemon. Save/quit/start is the daemon workflow.

There is no stock selector, restart manager, local Emacs patch stack, portable
distribution or release service. MSYS2/compiler upgrades may invalidate older
builds, so rebuilding is required; rolling dependencies are not bit-reproducible.
