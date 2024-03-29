test.comp.cat{
  import star.
  import star.json.
  import star.peg.
  import star.pkg.
  import star.resources.

  import star.compiler.catalog.
  import star.compiler.location.

  import star.script.

  main:()=>action[(),()].
  main()=>do{
    cj .=   """{
  	"content": {
  		"test.comp.op" : "optest.star",
  		"test.comp.err" : "errtest.star",
  		"test.comp.cat" : "catalogtest.star"
  	},
  	"version": "1.0.0"
    }"""::json;

    show "json is $(cj)";

    cc .= parseCat(cj,cwd());

    show "catalog is $(cc)"
  }
}
