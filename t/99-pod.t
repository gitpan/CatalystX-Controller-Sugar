#!/usr/bin/perl
use lib qw(lib);
use Test::More;
eval 'use Test::Pod or plan skip_all => 'Test::Pod required';
all_pod_files_ok();
