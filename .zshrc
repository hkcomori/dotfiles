# .zshrc

REPORTTIME=3    #N秒以上かかったプロセスの所要CPU時間を表示

# カラー表示
export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:';

# Completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'     # 補完時に大小文字を区別しない
zstyle ':completion:*' menu select=1
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}   # ファイルリスト補完でもlsと同様に色をつける｡
zstyle ':completion:*:default' menu select              # arrow keyで候補間移動
zstyle ':completion:*' completer _complete _approximate _prefix     # 単語途中の補完を有効化する
setopt magic_equal_subst        # コマンドラインの引数で --prefix=/usr などの = 以降でも補完できる
setopt mark_dirs                # ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt auto_param_keys          # カッコの対応などを自動的に補完
setopt auto_param_slash         # ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える

autoload -U compinit && compinit -u

# Keybind
bindkey -e
bindkey "^?" backward-delete-char
bindkey "^H" backward-delete-char
bindkey "^[[3~" delete-char
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line
bindkey "^[[5~" history-beginning-search-backward
bindkey "^[[6~" history-beginning-search-forward
bindkey "^r" history-incremental-search-backward
bindkey "^s" history-incremental-search-forward
bindkey "^R" history-incremental-search-backward
bindkey "^S" history-incremental-search-forward

WORDCHARS=${WORDCHARS:s,/,,}    # '/'も単語区切りとみなす。

# History
setopt append_history           # 履歴を追加 (毎回 .zhistory を作るのではなく)
setopt hist_ignore_all_dups     # 重複するコマンド行は古い方を削除
setopt hist_reduce_blanks       # 余分な空白は詰めて記録
setopt share_history            # 履歴の共有
setopt hist_verify              # ヒストリを呼び出してから実行する間に一旦編集できる状態にする
setopt extended_history         # 履歴ファイルに時刻を記録
HISTFILE="$HOME/.zsh.d/history"
HISTSIZE=32768
SAVEHIST=32768

setopt no_flow_control          # C-s/C-q によるフロー制御を使わない
setopt AUTO_PUSHD               # cdしたらカレントディレクトリをpushdする
setopt autocd                   # コマンド名が無い場合はその名前でcdを試みる

# Alias
if [ -f ~/.aliases ]; then
    source ~/.aliases
fi

# Prompt
if [ -f ~/.zsh.d/prompt ]; then
    source ~/.zsh.d/prompt
fi
