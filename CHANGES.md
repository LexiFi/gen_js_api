Changelog
=========

Version 1.1.0
-------------

- GPR#164:  Switch to js_of_ocaml.4.0 (@hhugo) 
- GPR#165:  Allow n-ary constructors in [@js.union] (@cannorin)

Version 1.0.9
-------------

- GPR#161: Fix broken link to VALUES.md (@joelburget)
- GPR#154: Upgrade to ocaml/setup-ocaml@v2 (@smorimoto)
- GPR#153: Support recursive modules (@cannorin)
- GPR#152: [@@js.invoke] attribute to call the global object as a function (@cannorin)

Version 1.0.8
-------------

- GPR#149: Stop using OMP directly (@mlasson)
- GPR#145: Add support for "newable" functions to [@@js.apply] (@cannorin)
- GPR#143: Disable eta reduction for of_js and to_js of type aliases (@cannorin)
- GPR#144: Disable "Spurious js.\* attribute" error for @js.dummy (@cannorin, @mlasson)
- GPR#146: Fix an edge-case bug of prepare_args


Version 1.0.7
-------------

- GPR#140: Adds a deprecation warning the automatic heuristic is used (@mlasson)
- GPR#139: Rename things for backward compatibility (@mlasson)
- GPR#135: UTF-8 support for (Ojs.get/set/delete) adaptions (@mlasson)
- GPR#132: Add support for indexers and "callable" objects (@cannorin)
- GPR#130: Javascript -> JavaScript (@smorimoto)
- GPR#129: Add GitHub Actions workflow (@smorimoto)
- GPR#128: Bucklescript -> ReScript (also add genType ppx as a resource) (@ryyppy)
- GPR#127: Support boolean "enum"s and boolean union discriminators (@cannorin)
- GPR#125: js.custom attribute for type declaration to support custom mapping #125 (@cannorin)
- GPR#123: Upgrade ppx to the ocaml 4.11 ast (@hhugo)
- GPR#120: Split runtime library to own package (@rgrinberg)
- GPR#118: Add ppx tests setup (@jchavarri, @mlasson)
- GPR#115: Support for functors and module inclusion (@mlasson)
- GPR#114: Dependency tweaks (@rgrinberg)
- GPR#113: Add support for type variables (@jchavarri, @mlasson)
- GPR#111: Better ppxlib integration (@hhugo)
- GPR#110: Include payload in extension node (@nojb)

Version 1.0.6
-------------

- GPR #101: Adds travis support + use ocaml-migrate-parsetree (@mlasson)
- GPR #94: Typo: correct wrong 'apply_arr' to 'apply' (@facelesspanda)
- GPR #89: Update the opam file (@hhugo)
- GPR #87: Switch to dune (@hhugo)
- GPR #88: Fix some warnings (@hhugo)
- GRP #85: Adapt to 4.08 (@nojb)

Version 1.0.5
-------------

- Adapt to OCaml 4.06


Version 1.0.4
-------------

- Adapt to OCaml 4.05.
