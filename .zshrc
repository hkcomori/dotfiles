bindkey -e
PATH=$HOME/bin:$HOME/bin/copy:/sbin:/usr/sbin:/usr/local/sbin:$PATH
#PS1=$'%n@%m:%~\n%(!.#.$) '

[ -f ~/.zsh.d/prompt ] && source ~/.zsh.d/prompt
[ -f ~/.zsh.d/functions ] && source ~/.zsh.d/functions

#PROMPT='['${USER}'@'${HOST}'] %~ '$'\n''%(!.#.$) '

REPORTTIME=5      #N秒以上かかったプロセスの所要CPU時間を表示

export EDITOR='/usr/bin/emacs -nw'

preexec () {
  [ ${STY} ] &&   
}

source ~/.aliases

## カラー表示
autoload colors && colors
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'

## 補完時に大小文字を区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' menu select=1
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} # ファイルリスト補完でもlsと同様に色をつける｡
#zstyle ':completion:*::::' completer _complete _migemo_complete # migemo日本語補完
zstyle ':completion:*:default' menu select # arrow keyで候補間移動
setopt extended_history # 履歴ファイルに時刻を記録
setopt BASH_AUTO_LIST
setopt LIST_AMBIGUOUS
setopt AUTO_PUSHD # cdのタイミングで自動的にpushd
setopt autocd                # ディレクトリのみで移動
setopt magic_equal_subst # コマンドラインの引数で --prefix=/usr などの = 以降でも補完できる
setopt mark_dirs # ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt auto_param_keys # カッコの対応などを自動的に補完
setopt auto_param_slash # ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt no_flow_control       # C-s/C-q によるフロー制御を使わない
setopt ignore_eof            # C-dでログアウトしない
setopt print_eight_bit

autoload -U compinit && compinit -u

## keybind
bindkey -e
bindkey "^?"    backward-delete-char
bindkey "^H"    backward-delete-char
bindkey "^[[3~" delete-char
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line
bindkey "^[[5~" history-beginning-search-backward
bindkey "^[[6~" history-beginning-search-forward
bindkey "^r" history-incremental-search-backward
bindkey "^s" history-incremental-search-forward
bindkey "^R" history-incremental-search-backward
bindkey "^S" history-incremental-search-forward

WORDCHARS=${WORDCHARS:s,/,,} # '/'も単語区切りとみなす。

setopt append_history        # 履歴を追加 (毎回 .zhistory を作るのではなく)
setopt hist_ignore_all_dups  # 重複するコマンド行は古い方を削除
setopt hist_reduce_blanks    # 余分な空白は詰めて記録
setopt share_history         # 履歴の共有
setopt hist_verify # ヒストリを呼び出してから実行する間に一旦編集できる状態になる

## history
HISTFILE="$HOME/.zsh.d/history"
HISTSIZE=32768
SAVEHIST=32768

#if [ $SHLVL = 1 ] && [ `pwd` = $HOME ]; then
#    screen -x || screen -R
#elif [ $SHLVL = 1 ] && [ $HOST = "energy" ]; then
#    cd ~/analyze/Matlab_Scripts
#fi

if [[ $EMACS = "" ]]; then
    if [[ $TERM != "screen" ]]; then
        # setting for no screen
        #if [ `ps uwx| grep 'emacs --daemon'| wc -l` -lt 2 ]; then
    	#/usr/bin/emacs --daemon
        #fi
        if [ `pwd` = $HOME ]; then
    	screen -x || screen -R
        fi
    else
        # setting for screen
        preexec() {
    	#echo -ne "\ek${1%% *}\e\\"
    	lastcmd="$1               "
    	#echo -ne "\ek$lastcmd[1,10]\e\\"
    	echo -ne "\ek$lastcmd[1,15]\e\\"
        }
        echo -ne "\eknew_window     \e\\"
    fi
fi
