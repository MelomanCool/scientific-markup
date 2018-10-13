# Настраиваемая и расширяемая разметка для научных работ

Меня давно уже посещают мысли о простой, но достаточно гибкой разметке для оформления научных работ.

На первый взгляд, Markdown достаточно для структурирования большей части текста научной работы (кроме титульника, содержания, списка литературы и, возможно, приложений).

Научные работы оформляются по ГОСТу, который требует ссылаться на рисунки, таблицы и тому подобное в предшествующем им абзаце. Этим можно злоупотребить, и вынести такие элементы из Markdown, после чего каким-либо образом вставлять их в результирующий документ (используя макросы LaTeX?). Хочется иметь возможность делать ссылку на документ, возможно, указывая, вставить его прямо здесь — или сразу после абзаца.

В идеале, разметка должна позволять вставлять в документ что угодно, что можно как-то сконвертировать в поддерживаемый формат. Например, если поддерживаются только растровые картинки — конвертировать в них и таблицы, и векторные изображения, и все остальное, что нельзя представить просто в виде текста. Конвертация должна выполняться автоматически (может быть, будет достаточно и правила в Makefile).

Вообще, хотелось бы иметь вариант, позволяющий не думать об оформлении вообще (хотя и оставляя возможность это делать, при необходимости), но сфокусироваться на самом содержании работы. Помечать, что здесь — заголовок, а здесь — рисунок, а об оформлении забыть, либо заниматься им отдельно, редактируя стили определенных блоков (или чего-то вроде них).


## Необходимые элементы
- рисунки
- таблицы
- формулы


## Проблемы
- оформление конечного документа должно соответствовать ГОСТу
- собственно, способ встройки элементов вроде рисунков и таблиц в работу
- способ создания таблиц должно быть максимально удобным, и в то же время гибким
- автоматическое оформление списков


### Оформление согласно ГОСТу
#### Markdown

Предположительно, для этой цели можно создать шаблон Pandoc, соответствующе его настроив.

Как вариант, можно попробовать сконвертировать Markdown в LaTeX, и использовать готовые шаблоны уже для него.

Недавно, появилась такая вещь как [GOSTdown](https://gitlab.iaaras.ru/iaaras/gostdown/). Однако, она намертво привязана к Word (и, соответственно, к Windows), поскольку использует вордовский COM-интерфейс.


#### LaTeX

Для LaTeX, существует несколько шаблонов для написания научных работ. Из проверенных, у меня заработал лишь этот: https://github.com/AndreyAkinshin/Russian-Phd-LaTeX-Dissertation-Template.


### Cоздание таблиц

Таблицы в Markdown создавать довольно просто, но только если суммарный текст в строке достаточно короткий, чтобы помещаться на экране. Проблема заключается в том, что исходный текст внутри ячейки нельзя разбивать на несколько строк, поскольку перенос строки используется для отделения строк таблицы.

Возможные решения:
- таблицы Pandoc Markdown (как будут вести себя $$-формулы?)
- вынесение длинного текста из таблицы с помощью макросов LaTeX или переменных Pandoc
- использование LaTeX для разметки таких сложных таблиц, со всеми вытекающими последствиями

LaTeX, в свою очередь, позволяет оформлять исходный текст для таблиц каким угодно образом, но синтаксис у него слишком уж громоздкий и, местами, низкоуровневый (например, нужно вставлять в конце каждой строки \hline для добавления линии между строками).

Возможные решения:
- макросы
- пакеты для таблиц


### Автоматическое оформление списков

Элементы списков, по ГОСТу, должны быть оформлены следующим образом:
- текст всех элементов, кроме последнего, заканчивается точкой с запятой
- текст последнего элемента заканчивается точкой

И не очень удобно за этими вещами следить, особенно при добавлении еще одного элемента в конец списка. Было бы удобнее вообще ничего не оставлять в конце строки, и возложить эту задачу на компьютер.

Помимо этого, ГОСТ также требует, чтобы текст элементов начинался со строчной буквы (если это не аббревиатура или нечто подобное, обязательно начинающееся с заглавной). Но это, по крайней мере, легче заметить, и поэтому не столь критично.


## Мысли
### Инструменты/разметки для данной задачи

Следует исследовать:
- Pandoc
- LaTeX
- Asciidoctor
- Pollen
- HTML + CSS


#### Pandoc

Инструмент для конвертации документов из одного формата в другой. Поддерживает большое количество форматов.

Наиболее интересным для нашей задачи является его собственный вариант Markdown.

Имеет возможность создавать свои [фильтры](https://pandoc.org/filters.html) на [Haskell](https://pandoc.org/filters.html#a-simple-example), а также [Python, Lua и еще ряде языков](https://pandoc.org/filters.html#but-i-dont-want-to-learn-haskell). Фильтры преобразуют AST, полученный путем разбора исходного файла Pandoc'ом.

Это является и плюсом, и минусом фильтров. Плюс заключается в том, что не приходится работать с сырым текстом. Минус же — в том, что фильтры не позволяют (по крайней мере, легко) добавлять новый синтаксис, поскольку предназначены для преобразования существующих элементов документа.


#### LaTeX

Определенно позволяет оформить работу по ГОСТу, но, при этом, сам по себе сравнительно сложен для изучения.

Расширения можно создавать на самом LaTeX. Не совсем понятно, существует ли какой-то уровень абстракции над текстом (списки строк, например).

[Плейлист по LaTeX на YouTube](https://www.youtube.com/playlist?list=PL-p5XmQHB_JSQvW8_mhBdcwEyxdVX0c1T).


#### Asciidoctor

Текстовый процессор для AsciiDoc с некоторыми плюшками.

Сам AsciiDoc, на первый взгляд, напоминает Markdown.

Можно делать [расширения](https://asciidoctor.org/docs/user-manual/#extensions) на Ruby. Расширения работают на разных уровнях — сырой исходник, AST, уже обработанный документ, помеченный специальным образом блок текста, и пара других.


#### Pollen

Pollen позволяет встраивать в текстовый файл пользовательские теги и применять функции/макросы, написанные Racket, да и вообще пользоваться всем инструментарием Racket. Разметка Pollen напоминает язык шаблонов, но при этом позволяет использовать полноценный язык программирования, вместо урезанного.

Что мне очень нравится:
- пользовательские теги
- полноценный язык программирования, встраиваемый в любой текстовый формат документов

Обе эти функции не то что доступны из коробки — они преподносятся как единственный возможный вариант использования Pollen.

Инструмент это до ужаса простой, и настолько же гибкий. Разметка Pollen не имеет каких-либо предопределенных тегов или синтаксиса для форматирования текста, вроде **жирного** или *курсива*. Вместо этого, Pollen предлагает пользователю самостоятельно создать набор необходимых тегов и функций (или макросов), и затем придать им определенный смысл для каждого выходного формата документа.

По сути, Pollen встраивает язык Racket в обычные текстовые документы. Это, конечно, не означает, что функции необходимо определять прямо в исходных документах. Отдельно хочется отметить тот интересный факт, что Pollen умеет работать с любыми текстовыми форматами. Из исходных форматов, разбирать он умеет, насколько я понял, только Markdown. Из выходных форматов, поддерживается HTML.

В то же время, генерировать Pollen может разметку на любом языке. Чтобы этого достичь, Pollen позволяет делать следующее:
- вешать на теги функции, предназначенные для преобразования исходного текста во что-либо другое. Например, функция может возвратить HTML-тег или блок кода на LaTeX
- вешать функции на элементы определенного типа,

Вот пример размеченного Pollen файла:

```
#lang pollen

◊heading{Brennan Huff}
 
Today is ◊(get-date). I ◊emph{really} want this job.
```

Символ `◊` помечает теги. Что это за теги, и как их можно использовать? Самое интересное — теги могут быть какими угодно (в пределах поддерживаемого набора символов, конечно). Позже, можно определить для каждого выходного формата, каким образом эти теги будут обрабатываться.

Для этого, на теги можно вешать функции, написанные на языке Racket. Достаточно определить функцию с таким же, как у тега, названием:

```racket
#lang racket/base
(require racket/date txexpr)
(provide (all-defined-out))
 
(define (get-date)
  (date->string (current-date)))
 
(define (heading . elements)
  (txexpr 'h2 empty elements))
 
(define (emph . elements)
  (txexpr 'strong empty elements))
```

Как видно в случае с `get-date`, функции могут использоваться, помимо обработки текста, и для других целей.

Функции `heading` и `emph` преобразуют пользовательские теги в конкретные теги HTML, но возвращают не строку с HTML, а [X-expression](https://docs.racket-lang.org/pollen/programming-pollen.html#%28part._.Returning_an_.X-expression%29) (которое, по сути, аналогично тегу XML).

Pollen умеет преобразовывать такие выражения в HTML. В результате, получится следующее (без учета шаблона):

```
<h2>Brennan Huff</h2>
 
Today is October 1st, 2018. I <strong>really</strong> want this job.
```

Этот же исходный файл, без каких-либо изменений, можно преобразовать, например, в простой текст. Достаточно изменить функции тегов таким образом, чтобы для текстовых файлов они возвращали строку, а не X-expression. Также, необходимо указать интересные нам форматы выходных файлов: `(define poly-targets '(html txt))`.

```racket
#lang racket/base
(require racket/date txexpr pollen/setup)
(provide (all-defined-out))
 
(module setup racket/base
  (provide (all-defined-out))
  (define poly-targets '(html txt)))
 
(define (get-date)
  (date->string (current-date)))
 
(define (heading . elements)
  (case (current-poly-target)
    [(txt) (map string-upcase elements)]
    [else (txexpr 'h2 empty elements)]))
 
(define (emph . elements)
  (case (current-poly-target)
    [(txt) `("**" ,@elements "**")]
    [else (txexpr 'strong empty elements)]))
```

Результат выглядит следующим образом:

```
BRENNAN HUFF

Today is Monday, October 1st, 2018. I **really** want this job.
```

TODO: Упомянуть в начале, что удобочитаемая разметка в исходном документе не является приоритетом. Гораздо важнее иметь возможность добавлять свои "теги" или нечто вроде, для пометки блоков текста определенным образом.


## Результат

Решил остановиться на Pollen, поскольку он лучше всего соответствует моей изначальной задумке — настраиваемой и расширяемой Markdown-подобной разметке. Благодаря этому, он позволяет вынести из документа "умный" (по сравнению с разметкой) код. В документе остается только суть — сам текст и теги, помечающие части этого текста тем или иным образом.

Помимо этого, изначально, документ декодируется в структуру данных (дерево), которую можно преобразовывать и анализировать. Скорее, даже нужно, поскольку сам Pollen поддерживает только конвертацию этой структуры в HTML. Вывод в других форматах нужно реализовывать самостоятельно, но на это Pollen и расчитан.

Про анализ стоит отдельно упомянуть. Так как документ преобразовывается в дерево, ничего не стоит пройтись по нему и вытянуть список нужных тегов. К примеру, собрать список всех заголовков, чтобы построить содержание. Понадобился список ссылок — пожалуйста. И все в этом духе.

Конечно, исходный файл в Pollen Markup выглядит не так читабельно, как Markdown-файл. Но это и не являлось моим приоритетом. Файл должен выглядеть "достаточно читабельно", но не обязательно выглядеть в исходном виде похожим на результирующий файл образом.

Чтобы проиллюстрировать, что я имею ввиду под "достаточно читабельно", я могу привести языки, которые считаю недостаточно читабельными: LaTeX, XML/HTML. LaTeX очень крут для рендеринга матана, но для обычных вещей довольно громоздок, на мой взгляд. Почему я XML/HTML считаю нечитабельными, думаю, пояснять не нужно.

Помимо этих двух языков, я, до того, как нашел Pollen, рассматривал написание работ сразу в структурированном виде. Например, в s-expressions или в JSON. Это бы очень упростило разбор документа, но читабельным я это уже точно не назвал.

Самым читабельным языком я бы назвал Markdown и ему подобные. Но все они очень ограничены.

Pollen ближе всего именно к Markdown-подобным языкам — текст первоочереден, разметка его дополняет. Только эта разметка не ограничена практически ничем. Нужен синтаксис для таблиц? Пожалуйста — напиши небольшую [функцию](https://docs.racket-lang.org/pollen-tfl/_pollen_rkt_.html#%28elem._%28chunk._~3cquick-table~3e~3a1%29%29) и помести таблицу в тег с именем этой функции. Нужны заголовки? Придумай название для тегов (к примеру, "heading" и "chapter") и определи, во что должно преобразовываться содержимое этих тегов. Готово.

К слову, никто не запрещает использовать готовые решения. Сам Pollen имеет в своей [библиотеке](https://docs.racket-lang.org/pollen/Typography.html) (хотя и в нестабильной её части, на момент написания этих слов) функции для преобразования `---` в "—", обычных кавычек — в красивые. На мой взгляд, нам нужно больше подобных изолированных функций.

Существует огромное количество монолитных программ, не позволяющих использовать их части по отдельности. Я говорю о программах для рендеринга Markdown. Хотя разметка и состоит из разных элементов, вы не сможете, скажем, найти отдельную функцию, которая преобразует Markdown-список в HTML-список. Я уже молчу о получении этого списка в виде структуры данных. Но это не говорит о том, что подобные функции трудно реализовать. Выше я уже привел пример — функция, позволяющая размечать таблицы практически как в Markdown.


## Вывод

Пора перестать реализовывать "Markdown flavours", каждый со своим набором расширений, которые нужны, в первую очередь, их авторам. Настало время модульных, расширяемых разметок.

Можно их назвать "Domain-Specific-Markups", поскольку они, в самом деле, могут быть настроены под каждую конкретную задачу, связанную с документами.

Для этого, существует инструмент — Pollen. Как заявляет сам автор, Pollen не идеален, но в то же время уже полностью готов к использованию.
