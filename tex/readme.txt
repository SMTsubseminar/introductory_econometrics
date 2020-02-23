以下の内容をホームディレクトリの.latexmkrcに書き込む(ない場合は作成する):
```
#!/usr/bin/env perl


$lualatex = 'lualatex -synctex=1 -halt-on-error %O %S';
$bibtex_use = 2;
$biber = 'biber %O --bblencoding=utf8 -u -U --output_safechars %B';
$max_repeat = 5;
```
