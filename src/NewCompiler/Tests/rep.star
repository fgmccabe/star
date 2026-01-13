test.comp.repo{
  import star.

  import star.resources.
  import star.uri.

  show disp(searchForRsRc(^parseUri("file:"++_cwd()),".star-repo/")).
}
