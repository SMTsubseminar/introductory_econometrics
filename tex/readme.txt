macOS・linuxユーザは以下の内容を~/.latexmkrcに書き込み，`latexmk -lualatex hoge.tex`とすることで1コマンドでbiblatexの処理まで実行できる．lualatexの部分を適宜platexなどに変更することで任意のエンジンで使用できるはず．


```latexmkrc
#!/usr/bin/env perl

$lualatex = 'lualatex -synctex=1 -halt-on-error %O %S';
$bibtex_use = 2;
$biber = 'biber %O --bblencoding=utf8 -u -U --output_safechars %B';
$max_repeat = 5;
```

詳細はTex wikiの説明を参照されたい:https://texwiki.texjp.org/?Latexmk
