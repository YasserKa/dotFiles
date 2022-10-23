syn keyword i3ConfigIncludeKeyword include contained
syn match i3ConfigInclude /^\s*include\s\+.*$/ contains=i3ConfigIncludeKeyword

hi def link i3ConfigIncludeKeyword                  Identifier
hi def link i3ConfigInclude                         Variable
