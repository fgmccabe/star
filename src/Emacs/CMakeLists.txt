cmake_minimum_required(VERSION 3.20)
project(Emacs)

include(./emacs.cmake)

add_emacs(star-util star star-config star-indent star-repo star-ops star-shell)
