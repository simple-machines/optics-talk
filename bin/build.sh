#!/bin/sh

dist_dir=dist
src_dir=src
class_dist_dir=${dist_dir}/classes

mkdir -p ${dist_dir}
mkdir -p ${class_dist_dir}

scalac -d ${class_dist_dir} ${src_dir}/*.scala

