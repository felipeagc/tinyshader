#!/bin/bash
cat tinyshader/*.h tinyshader/*.c | sed '/^#include "/d' > ./tinyshader_combined.c
