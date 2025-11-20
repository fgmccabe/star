test.fil{
  import star.
  import star.io.
  import star.assert.

  -- Test getting a file

  main:(){}.
  main(){
    try{
      show rdFile(".",.utf8Encoding)
    } catch {
      M do {
	assert .notFound .= M
      }
    }
  }
}
