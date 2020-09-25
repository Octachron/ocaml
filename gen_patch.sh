#!/usr/bin/env bash

weekly=~/ocaml/merlin_weekly
stable=~/ocaml/11
merlin=~local/merlin

function gen {
	merlinname=$1
	compilername=2
	mkdir patches/$compilername -p
	for i in $merlin/src/ocaml/$merlinname/411/*.ml{,i}
	do
		name=${i##*/}
		if   [ -f "$stable/$compilername/$name" ]; then
			patchname=patches/$compilername/$name.patch
			diff -ud  $i $stable/$compilername/$name > $patchname
			if [ ! "$(cat $patchname)" ]; then
				rm $patchname
				echo empty diff $name
			else
				echo $compilername $name
			fi
		fi
	done
}

function promote {
	merlinname=$1
	compilername=$2
	old=411
	new=412
	mkdir $merlin/src/ocaml/$merlinname/$new
	for i in $merlin/src/ocaml/$merlinname/$old/*.ml{,i}
	do
		name=${i##*/}
		if   [ -f "$weekly/$compilername/$name" ]; then
			cp $compilername/$name ${i/$old/$new}
#			git diff --no-index $i $stable/$2/$name > $patchname
	   else
			cp $i ${i/$old/$new}
		fi
	done

}

function gen_all {
	gen typing typing
	gen typing file_formats
	gen parsing parsing
	gen utils utils
}

function promote_all {
	promote typing typing
	promote typing file_formats
	promote parsing parsing
	promote utils utils
}


function apply {
	for i in patches/**/*.patch
	do
		patchname=${i#patches/}
		name=${patchname%.*}
		patch -R $name patches/$patchname
	done
}

promote_all
