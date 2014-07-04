breed [nodos nodo]
breed [vehiculos vehiculo]
breed [obstáculos obstáculo] ;deja pasar, disminuyendo velocidad
breed [barreras barrera] ; no deja pasar ;cambia de carril, de ser posible
breed [semáforos semáforo]
breed [seudonodos seudonodo]
directed-link-breed [calles calle]
directed-link-breed [linksems linksem]
directed-link-breed [linkseudos linkseudo]
extensions [web]

globals [
  pnodo
  snodo
  un-nodo
  es-pnodo?
  sh-lnormal
  sh-lselec
  num-nodos-carga
  CERCA
  sh-origen
  sh-destino
  sh-nodo-normal
  fondo-actual
  muestra-red?
  reservar?
  tamaño-nodo
  velocidad-baja
  velocidad-detenido
  mensajitos?
  ;;
  sistema-de-semáforos
  color-default-arista
  color-default-nodo
  color-default-obstáculo
  color-default-barrera
  color-default-semáforo
  semáforos-visibles?
  mouse-soltado?
  mouse-presionado?
]
nodos-own [
  valor
  densidad
  seleccionado?
  seudos
]
semáforos-own [
  seleccionado?
  estado
]

vehiculos-own [
  metas
  velocidad-deseada
  siguiente-nodo  
  velocidad
  ;;
]
calles-own [
  semáforo-color-arista
  es-semáforo-arista?
  num-carriles
]
seudonodos-own [
  nodo-base
]

to idle
end

;por si seleccionan los separadores!!
to ACCIÓN-ACTUAL-------------
end

to inicializa-globales
  set sh-lnormal "default" ; forma de la arista "normal" (no seleccionada)
  set sh-lselec "selec" ; forma de la arista seleccionada
  set sh-origen "square"
  set sh-destino "square 2"
  set sh-nodo-normal "circle"
  set es-pnodo? false ; es primer nodo dentro de una selección multiple (p. ejem. para seleccionar una arista)
  set CERCA .7 ; define "cerca" para seleccionar, por ejemplo
  set acción-actual "añade-arista" ; var. que indica cual es la acción a ejecutar por el go
  set num-nodos-carga 0 ; ????
  set fondo-actual "" ; archivo del fondo actual ????
  set muestra-red? true ; se está mostrando la red? (nodos y aristas)
  set reservar? false
  set tamaño-nodo 1
  set velocidad-baja .125
  set velocidad-detenido 0
  set mensajitos? false ; MUESTRA MENSAJITOS
  set sistema-de-semáforos nobody
  set color-default-arista white
  set color-default-nodo 48
  set color-nodo color-default-nodo
  set color-default-barrera 125
  set color-default-obstáculo pink
  set color-default-semáforo red
  set-default-shape semáforos "emblem2"
  set semáforos-visibles? true
  set mouse-soltado? false
  set mouse-presionado? false
  set pnodo nobody
  set snodo nobody
end

to setup
  ca
  inicializa-globales
  set-default-shape nodos sh-nodo-normal ; pone la forma default de los nodos (nodo normal)
  set-default-shape calles sh-lnormal ; pone la forma default de las aristas
  set-default-shape vehiculos "car top" ; forma de los vehiculos
  set-default-shape obstáculos "target"
  set-default-shape barreras "x"
  reset-ticks
end

to añade-meta
  ;ask vehiculos [
    ;set meta (sentence meta (read-from-string nueva-meta))
    ;if (verbose) [ show (fput "meta =" meta ) ]
  ;]
end

to-report corre-t [tarea]
  report run-result (word "tarea-" tarea)
end

to obedecer
  if (not empty? metas) [
    let meta-actual (first metas)
    if meta-actual = ["die"] [die]
    ifelse not empty? meta-actual [
      let l length meta-actual
      let s 0
      foreach meta-actual [ if (corre-t ?) [set s s + 1] ]
      if s = l [ set metas (bf metas) ] ; sólo quita la meta-actual si TODAS sus tareas devuelven true
    ][set metas (bf metas)] ; .. o si no tiene nada
  ]
end

to-report tarea-die ;no hace nada si hay mas tareas en esta meta
  report True
end

to go
  if reservar? = 0 [setup]
  if avanza-vehiculos? [
      genera-vehiculos
      ;avanza-vehiculos
      ask vehiculos [ obedecer ]
  ]
  let tamaño2 2.3 * tamaño-nodo
  every .6 [
    let ns nodos-seleccionados
    if ns != nobody [
      ask ns [ set size tamaño2 - size ]
    ]
    let ss semáforos-seleccionados
    if ss != nobody [
      ask ss [set size tamaño2 - size ]
    ]
  ]
  cambia-semáforos
  run (word "acción-actual-" acción-actual)
  tick
end

to ajusta-parámetros-nodo [donde]
  ask donde [set seleccionado? false set densidad .2 set seudos [] ]
end

to ajusta-parámetros-barrera [donde]
end

to ajusta-parámetros-obstáculo [donde]
end

to ajusta-parámetros-semáforo [donde]
  ask donde [
    set seleccionado? false
    set estado []
    ask donde [
      let x xcor + 1
      let y ycor + 1
      if x > max-pxcor [ set x xcor - 1 ]
      if y > max-pycor [ set y ycor - 1 ]
      setxy x y
    ]
    vincula-semáforo-a-nodo donde nodo-cercano
  ]
end

to ajusta-parámetros [objeto donde]
  let s (word "ajusta-parámetros-" objeto " " donde)
  run s
end

to crea-objeto [objeto donde x y]; objeto: string
  let s (word "create-" objeto "s 1 [ set " donde " self setxy x y set color color-default-" objeto " set size tamaño-nodo]")
  run s
  ajusta-parámetros objeto donde
end

to crea-nodo [donde x y]
  crea-objeto "nodo" donde x y
end

to crea-semáforo [donde x y]
  crea-objeto "semáforo" donde x y
end

to crea-obstáculo [donde x y]
  crea-objeto "obstáculo" donde x y
end

to crea-barrera [donde x y]
  crea-objeto "barrera" donde x y
end

to acción-actual-añade [objeto]
  if mouse-inside? [
    if ratón-soltado? [
      crea-objeto objeto "pnodo" mouse-xcor mouse-ycor
      set pnodo nobody
      ratón-limpia
    ]
  ]
end

to acción-actual-añade-nodo
  acción-actual-añade "nodo"
end

to acción-actual-añade-obstáculo
  acción-actual-añade "obstáculo"
end

to acción-actual-añade-barrera
  acción-actual-añade "barrera"
end

to acción-actual-añade-semáforo
  acción-actual-añade "semáforo"
end

to colorea
  if mouse-down? [
    let c nodo-cercano
    if c != nobody [
      ask c [ set color color-nodo ]
    ]
  ]
end

to-report objeto-cercanoxy [objetos x y] ; objetos = agentset
  let cercano min-one-of objetos [distancexy x y]
  ifelse cercano != nobody and [distancexy x y <= CERCA] of cercano [ report cercano ] [ report nobody ]
end

to-report objeto-cercano [objetos]
  report objeto-cercanoxy objetos mouse-xcor mouse-ycor
end

to-report nodo-cercano
  report objeto-cercano nodos
end

to-report barrera-cercana
  report objeto-cercano barreras
end

to-report obstáculo-cercano
  report objeto-cercano obstáculos
end

to-report semáforo-cercano
  report objeto-cercano semáforos
end

to selecciona
  set seleccionado? true
  set es-pnodo? false
end

to deselecciona
  set seleccionado? false
  set size tamaño-nodo
  set es-pnodo? false
end

to toggle-selecciona
  ifelse seleccionado? [ deselecciona ][ selecciona ]
end

to acción-actual-selecciona [objeto]
  if mouse-inside? [
    if ratón-soltado? [
      set pnodo objeto-cercano objeto
      ifelse pnodo = nobody [
        if user-yes-or-no? "Deselecciona todos?" [ deselecciona-todo-nodo deselecciona-toda-arista ]
      ]
      [
        ask pnodo [
          toggle-selecciona
          set pnodo nobody
        ]
      ]
      ratón-limpia
    ]
  ]
end

to acción-actual-selecciona-nodo
  acción-actual-selecciona nodos
  ;ask one-of 
end

to acción-actual-selecciona-nodo1
  if mouse-inside? [
    ifelse not reservar? [
      if mouse-down? [
        let c nodo-cercano
        ifelse c != nobody [
          ask c [
            set es-pnodo? true
            set pnodo self
            set reservar? true
          ]
        ]
        [ ; si apreté "al aire" deselecciona todo PERO bajo confirmación
          if user-yes-or-no? "Deselecciona todos?" [ deselecciona-todo-nodo deselecciona-toda-arista ]
        ]
      ]
    ]
    [
      if not mouse-down? [
        set es-pnodo? false
        ask pnodo [
          ifelse seleccionado? [ deselecciona ] [ selecciona ]
          ;ajusta valores de controles de nodos seleccionados
          set color-nodo color
          set densidad-nodo-actual densidad
          set nodo-actual-x xcor
          set nodo-actual-y ycor
        ]
        set reservar? false
        set pnodo nobody
      ]
    ]
  ]
end


to-report nodos-seleccionados 
  report nodos with [ seleccionado? ]
end

to-report semáforos-seleccionados 
  report semáforos with [ seleccionado? ]
end

to-report nodos-no-seleccionados
  report nodos with [ not seleccionado? ]
end

to selecciona-todo-nodo
  ask nodos [ selecciona ]
end

to deselecciona-todo-nodo
  ask nodos [ deselecciona ]
end

to selección-todos-los-nodos
  ifelse any? nodos-no-seleccionados
  [ selecciona-todo-nodo ]
  [ deselecciona-todo-nodo ]
end

to-report aristas-seleccionadas
  report calles with [shape = sh-lselec]
end

to-report aristas-no-seleccionadas
  report calles with [shape != sh-lselec]
end

to selección-borra-selección
  ask nodos-seleccionados [ die ]
  ask aristas-seleccionadas [ die ]
end

to defaults-para-nuevas-aristas
  ;selecciona-esta-arista
  set color color-default-arista
  set semáforo-color-arista color
  set es-semáforo-arista? false
  set num-carriles 1
end

to my-create-calle-to [o]
  create-calle-to o [defaults-para-nuevas-aristas]
end

to my-create-calle-from [o]
  create-calle-from o [defaults-para-nuevas-aristas]
end

to aristaBi [otroNodo]
  my-create-calle-to otroNodo
  my-create-calle-from otroNodo
end

to-report ratón-soltado?
  ifelse mouse-down? 
  [ set mouse-presionado? true set mouse-soltado? false ]
  [if mouse-presionado? [set mouse-soltado? true set mouse-presionado? false]] ;hacer mouse-soltado? false tan pronto se deje de usar
  report mouse-soltado?
end

; usar siempre la estructura
; if ratón-soltado? [
;   ...
;   ... operaciones a realizar si el ratón se soltó ...
;   ...
;   ratón-limpia ; para que no vuelva a "detectar" que el ratón se soltó
; ]
to ratón-limpia
  set mouse-soltado? false
  set mouse-presionado? false
end

to acción-actual-añade-arista
  if mouse-inside? [
    ifelse pnodo = nobody [ ; empieza creacion de arista
      if ratón-soltado? [
        set pnodo nodo-cercano
        if pnodo = nobody [ ; si no hay nodo cercano
          crea-nodo "pnodo" mouse-xcor mouse-ycor  ;créalo
        ]
        ask pnodo [selecciona] ; pnodo es el nodo cercano o uno creado si no había cercano, lo selecciona
        ratón-limpia
      ]
    ]
    [ ; ya hubo un pnodo
      if snodo = nobody [ ; si aún no hay segundo nodo
        if ratón-soltado? [
          set snodo nodo-cercano
          ifelse snodo = nobody [ ; click al aire, créa nuevo snodo
            crea-nodo "snodo" mouse-xcor mouse-ycor
            ask snodo [
              if (mensajitos?) [show "segundo nodo"]
              ifelse bi? [aristaBi pnodo][ my-create-calle-from pnodo ]
              ask pnodo [deselecciona]
              set pnodo nobody
              set snodo nobody
            ]
          ]
          [; click sobre nodo existente
            ifelse pnodo = snodo [ ; click sobre pnodo no permitido (loop), empieza desde principio
              ask pnodo [deselecciona]
              set pnodo nobody
              set snodo nobody
            ]
            [
              ask snodo [
                if (mensajitos?) [show "segundo nodo"]
                ifelse bi? [aristaBi pnodo][ my-create-calle-from pnodo ]
                ask pnodo [deselecciona]
                set pnodo nobody
                set snodo nobody
              ]
            ]
          ]
          ratón-limpia
        ]
      ]
    ]
  ]
end

to acción-actual-invierte-sentido-de-calle
  if mouse-inside? [
    if ratón-soltado? [
      ask calle-cercana mouse-xcor mouse-ycor [
        let e2 end2
        ask end1 [my-create-calle-from e2]
        die
      ]
      ratón-limpia
    ]
  ]
end

to vincula-semáforo-a-nodo [sem nodo]
  if nodo != nobody [
    ask nodo [
      set reservar? false
      create-linksem-to sem
      ask nodo [ ask linksem who [who] of sem [tie] ]
      deselecciona
    ]
  ]
  ask sem [ selecciona ]
end

to borra-aristas-de-selección
  let s aristas-seleccionadas
  if s != nobody [
    ask s [
      die
    ]
  ]
end

to acción-actual-añade-nodo-a-arista
  if mouse-inside? [
    if ratón-soltado? [
      let x mouse-xcor
      let y mouse-ycor
      let s calle-cercana x y
      if s != nobody [
        crea-nodo "pnodo" x y
        ask s [
          ask end1 [ if bi? [my-create-calle-from pnodo] my-create-calle-to pnodo ]
          ask end2 [ if bi? [my-create-calle-to pnodo] my-create-calle-from pnodo ]
          die
        ]
      ]
      ratón-limpia
      set pnodo nobody
    ]
  ]
end

to selección-intercambia-nodos
  let n nodos-no-seleccionados
  deselecciona-todo-nodo
  ask n [ selecciona ]
end


to invierte-direccion-aristas-seleccionadas
  ask aristas-seleccionadas [
    let e1 end1
    let e2 end2
    ask e1 [
      let arista calle [who] of e1 [who] of e2
      ifelse is-calle? arista [ask arista [die] my-create-calle-from e2 ]
      [ask calle [who] of e2 [who] of e1 [die] my-create-calle-to e2 ]
    ]
  ]
  ;set accion-actual "selecciona"
end

to acción-actual-mueve [objeto]
  if mouse-inside? [
    ifelse ratón-soltado? [ ;se acaba el proceso
      set pnodo nobody
      ratón-limpia
    ]
    [
      ifelse pnodo = nobody [ ;si no se ha escojido objeto
        if mouse-down? [
          set pnodo objeto-cercano objeto
        ]
      ]
      [
        ask pnodo [setxy mouse-xcor mouse-ycor]
      ]
    ]
  ]
end
        

to acción-actual-mueve-nodo
  acción-actual-mueve nodos
end

to acción-actual-mueve-barrera
  acción-actual-mueve barreras
end

to acción-actual-mueve-obstáculo
  acción-actual-mueve obstáculos
end

to acción-actual-mueve-semáforo
  acción-actual-mueve semáforos
end

to acción-actual-borra [objeto]
  if ratón-soltado? [
    let c objeto-cercano objeto
    if c != nobody [
      ask c [ die ]
    ]
    ratón-limpia
  ]
end

to acción-actual-borra-nodo
  acción-actual-borra nodos
end

to acción-actual-borra-obstáculo
  acción-actual-borra obstáculos
end

to acción-actual-borra-barrera
  acción-actual-borra barreras
end

to acción-actual-borra-semáforo
  acción-actual-borra semáforos
  if mouse-down? [
    let c nodo-cercano
    if c != nobody [
      ask [out-linksem-neighbors] of c [die]
    ]
  ]
end

to acción-actual-mueve-nodos-seleccionados
  if mouse-inside? [
    ifelse not reservar? [
      if mouse-down? [
        let c nodo-cercano
        if c != nobody [
          ask c [
            set es-pnodo? true
            set pnodo self
            set reservar? true
          ]
        ]
      ]
    ]
    [
      set es-pnodo? false  
      let mx mouse-xcor - [xcor] of pnodo
      let my mouse-ycor - [ycor] of pnodo
      let minx max-pxcor
      let maxx min-pxcor
      let miny max-pycor
      let maxy min-pycor
      let ns nodos-seleccionados
      ask ns [
        if xcor + mx <= minx [set minx xcor + mx]
        if xcor + mx >= maxx [set maxx xcor + mx]
        if ycor + my <= miny [set miny ycor + my]
        if ycor + my >= maxy [set maxy ycor + my]
      ]
      if maxx <= max-pxcor and maxy <= max-pycor and minx >= min-pxcor and miny >= min-pycor [
        ask ns [
          setxy (xcor + mx) (ycor + my)
        ]
      ]
      ask vehiculos [face siguiente-nodo]
      if not mouse-down? [set reservar? false]
    ]
  ]
end

to-report link-distance [ x y ]
  let a [ distancexy x y ] of end1
  let b [ distancexy x y ] of end2
  let c link-length
  let d (0 - a ^ 2 + b ^ 2 + c ^ 2) / (2 * c)
  if d > c [
    report a
  ]
  if d < 0 [
    report b
  ]
  report sqrt (abs (b ^ 2 - d ^ 2) )
end

to-report arista-cercana [x y]
  report min-one-of links [link-distance x y]
end

to-report calle-cercana [x y]
  report min-one-of calles [link-distance x y]
end

to acción-actual-selecciona-arista
  if mouse-inside? [
    if ratón-soltado? [
      ask calle-cercana mouse-xcor mouse-ycor [
        toggle-selecciona-esta-arista
        let seleccionada? shape = sh-lselec
        ask both-ends [ifelse seleccionada? [selecciona][deselecciona]]
      ]
      ratón-limpia
    ]
  ]
end
  
to selección-añade-aristas-entre-nodos
  ask nodos-seleccionados [ ask other nodos-seleccionados [my-create-calle-from myself] ]
  deselecciona-todo-nodo
end

to selecciona-esta-arista ; en el contexto de arista a seleccionar
  set shape sh-lselec
end

to deselecciona-esta-arista ; en el contexto de arista a seleccionar
  set shape sh-lnormal
end

to toggle-selecciona-esta-arista
  ifelse shape = sh-lselec [set shape sh-lnormal][set shape sh-lselec]
end

to selecciona-toda-arista
  ask links [selecciona-esta-arista]
end

to deselecciona-toda-arista
  ask links [ deselecciona-esta-arista ]
end

to selección-todas-las-aristas
  ifelse any? aristas-no-seleccionadas
  [ selecciona-toda-arista ]
  [ deselecciona-toda-arista ]
end

to selección-intercambia-aristas
  let n aristas-no-seleccionadas
  deselecciona-toda-arista
  ask n [selecciona-esta-arista]
end

to kruskal
  deselecciona-toda-arista
  ask nodos [set valor who]
  foreach sort-on [link-length] calles [
    ask ? [
      let v0 [valor] of end1
      let v1 [valor] of end2
      if v0 != v1 [
        selecciona-esta-arista
        ask nodos with [valor = v1] [set valor v0]
      ]
    ]
  ]
end

to acción-actual-borra-arista
  if ratón-soltado? [
    deselecciona-toda-arista
    let cc calle-cercana mouse-xcor mouse-ycor
    if cc != nobody [ ask cc [ die ] ]
    ratón-limpia
  ]
end

to-report caracteristicas-nodo [n] ;reporta las caracteristicas a guardar de un nodo
  let car []
  ask n [
    set car (list (word "set color " color)
      (word "setxy " xcor " " ycor)
      (word "set densidad " densidad))
  ]
  report car
end

to ordena-valores-de-nodos
  let consecutivo 0
  foreach sort nodos [
    ask ? [
      set valor consecutivo
      set consecutivo consecutivo + 1
    ]
  ]
end

;to-report serializa-nodos ; convierte nodos en una lista para guardar en archivo
;  ordena-valores-de-nodos
;  report map [ caracteristicas-nodo ? ] sort turtles
;end
;
;to deserializa-nodos ; asume archivo abierto para lectura
;  let n file-read
;  create-nodos length n [
;    let car item (who - num-nodos-carga) n
;    foreach car [ run ? ]
;    selecciona
;  ]
;end
;    
;to-report caracteristicas-arista [a]
;  let car []
;  ask a [
;    set car (list (word "set color " color))
;    foreach sort both-ends [
;      ask ? [ set car lput who car ]
;    ]
;  ]
;  report car
;end
;  
;to-report serializa-aristas
;  report map [ caracteristicas-arista ? ] sort calles
;end

to-report arista-seleccionada? [a b]
  report [shape = "sh-lselec"] of calle a b 
end

;to-report revisa-carac-aristas [a]
;  let ladyac []
;  ask a [set ladyac [valor] of out-calle-neighbors]
;  report ladyac
;end
;
;to-report serializa-aristas2
;  report map [revisa-carac-aristas ?] sort nodos
;end
;
;to deserializa-aristas ;asume archivo abierto para lectura y que ya se han leido los nodos
;  let n file-read
;  foreach n [
;    let car ?
;    ask nodo (item 1 car) [
;      create-calle-with nodo (item 2 car) [
;        run (item 0 car)
;      ]
;    ]
;  ]
;end
;
;to deserializa-aristas2
;  let n file-read
;  let c num-nodos-carga
;  foreach n [
;    let vecinos ?
;    ask nodo c [ create-calles-to (turtle-set map [nodo (? + num-nodos-carga)] vecinos) [set shape sh-lselec]]
;    set c c + 1
;  ]
;end
;
;;reemplaza la ext1 por la ext2 (extensiones de nombre de achivo) del arch
;to-report reemplaza-ext [arch ext1 ext2]
;  let arch-sin remove ext1 arch
;  report (word arch-sin ext2)
;end

;to guardar-archivo
;  file-close
;  if (file-exists? archivo and user-yes-or-no? "Sobre-escribir archivo?") [
;    carefully [ file-delete archivo ] [ ]]
;  if (not file-exists? archivo) [
;    file-open archivo
;    file-write serializa-nodos
;    file-write serializa-aristas2
;    file-close
;    if fondo-actual != "" [
;      ask turtles [hide-turtle] ask calles [hide-link]
;      let fondo reemplaza-ext archivo ".dat" ".png"
;      export-view fondo
;      ask turtles [show-turtle] ask calles [show-link]
;    ]
;  ]
;end

;to cargar [acciones]
;  file-close
;  ifelse file-exists? archivo [
;    run acciones
;    file-open archivo
;    deserializa-nodos
;    deserializa-aristas2
;    file-close
;    let fondo reemplaza-ext archivo ".dat" ".png"
;    ifelse file-exists? fondo [
;      import-drawing fondo
;      set fondo-actual fondo
;    ]
;    [
;      set fondo reemplaza-ext archivo ".dat" ".jpg"
;      if file-exists? fondo [
;        import-drawing fondo
;        set fondo-actual fondo
;      ]
;    ]
;  ][user-message (word "Archivo " archivo " no existe")]
;end 
;  
;to cargar-archivo-old
;  cargar "setup"
;end

to carga-archivo
  setup
  if file-exists? archivo [
    import-world archivo
  ]
end

to carga-fondo
;  if file-exists? fondo [
;  import-drawing fondo
    web:import-drawing fondo
;    web:import-drawing-fine "http://wiki.inf.utfsm.cl/images/e/e8/Beastie.png" "head" []
 ;   import-drawing "prueba.png"
;    set fondo-actual fondo
;  ]
end

to guarda-archivo
   if (file-exists? archivo and user-yes-or-no? "Sobre-escribir archivo?") [
     carefully [ file-delete archivo ] [ ]]
   if (not file-exists? archivo) [
     export-world archivo
   ]
end

to selección-haz-orígenes
  let n nodos-seleccionados
  if any? n [
    ask n [ convierte-en-origen ]
  ]
end

to selección-haz-destinos
  let n nodos-seleccionados
  if any? n [
    ask n [ convierte-en-destino ]
  ]
end

to selección-haz-nodos-normales
  let n nodos-seleccionados
  if any? n [
    ask n [ convierte-en-nodo-normal ]
  ]
end

to genera-vehiculos
  ask nodos with [es-origen?] [
    if random-float 1 < densidad and not any? vehiculos-here [
        hatch-vehiculos 1 [ set size 1
          set siguiente-nodo one-of ([out-calle-neighbors] of myself)
          set color one-of base-colors
          show-turtle
          set velocidad-deseada (0.5 + random-float 1)
          set metas [ ["camaleato-basico"] ["die"] ]
        ]
    ]
  ]
end

to-report puede-avanzar?
  report not any? other vehiculos in-cone 1.5 40
end

to-report hay-obstáculo?
  report any? obstáculos in-cone 1 40
end

to-report hay-barreras?
  report any? barreras in-cone 1 40
end

to reemplaza-t [tarea-anterior tareas-nuevas] ; tarea-anterior es cadena, tareas-nuevas es lista o cadena
  let ma first metas
  set metas (bf metas)
  let nuevameta []
  foreach ma [
    ifelse ? = tarea-anterior
      [ set nuevameta (sentence nuevameta tareas-nuevas)]
      [ set nuevameta (sentence nuevameta ?) ]
  ]
  set metas (sentence (list nuevameta) metas)
end

to-report tarea-camaleato-basico
  reemplaza-t "camaleato-basico" ["pone-velocidad" "revisa-barreras" "puede-avanzar?" "revisa-obstáculos" "avanza-camaleato"]
  report False ; DEBE regresar False
end

to-report tarea-pone-velocidad ; ajusta la velocidad a la deseada
  set velocidad velocidad-deseada
  report True
end

to-report tarea-revisa-obstáculos
  if hay-obstáculo? [
    set velocidad velocidad-baja
    if (mensajitos?) [show "obstáculo"]
    report False
  ]
  report True
end

to-report tarea-revisa-barreras
  if hay-barreras? [
    set velocidad velocidad-detenido
    if (mensajitos?) [show "barrera" ]
    report False
  ]
  report True
end

to-report tarea-puede-avanzar?
  if not puede-avanzar? [
    set velocidad velocidad-detenido
    if (mensajitos?) [show "no puedo avanzar"]
    report False
  ]
  report True
end

to escoje-nuevo-nodo-camaleato
  set siguiente-nodo one-of ([out-calle-neighbors] of siguiente-nodo)
  if siguiente-nodo != nobody [
    face siguiente-nodo
  ]
end

to-report tarea-avanza-camaleato
  if siguiente-nodo = nobody [report True]
  face siguiente-nodo
  let d distance siguiente-nodo
  ifelse d > velocidad [ jump velocidad ] ; velocidad es la última metida en la pila
  [
    set velocidad d ; y si no falta mucho...;avanza el restito
    jump velocidad ;aquí ya llegamos a otro nodo
    ifelse nodo-es-destino? siguiente-nodo [ report True ]
      [ escoje-nuevo-nodo-camaleato ]
  ] 
  report False ; si no ha llegado al destino
end

to convierte-en-origen
  set shape sh-origen
end

to toggle-convierte-en-origen
  ifelse es-origen? [ convierte-en-nodo-normal ] [ convierte-en-origen ]
end

to convierte-en-destino
  set shape sh-destino
end

to toggle-convierte-en-destino
  ifelse es-destino? [ convierte-en-nodo-normal ] [ convierte-en-destino ]
end

to convierte-en-nodo-normal
  set shape sh-nodo-normal
end

to-report es-origen?
  report shape = sh-origen
end

to-report nodo-es-origen? [nodo]
  report [shape = sh-origen] of nodo
end

to-report es-destino?
  report shape = sh-destino
end

to-report nodo-es-destino? [nodo]
  report [shape = sh-destino] of nodo
end


to acción-actual-convierte-en-origen
  if mouse-inside? [
    if ratón-soltado? [
      set pnodo nodo-cercano
      if pnodo != nobody [
        ask pnodo [toggle-convierte-en-origen]
      ]
      ratón-limpia
    ]
  ]
end

to acción-actual-convierte-en-destino
  if mouse-inside? [
    if ratón-soltado? [
      set pnodo nodo-cercano
      if pnodo != nobody [
        ask pnodo [toggle-convierte-en-destino]
      ]
      ratón-limpia
    ]
  ]
end

to detecta-origenes-destinos ;colorea autom nodos detectados como fuentes y destinos
  deselecciona-todo-nodo
  deselecciona-toda-arista
  ask nodos with [count in-calle-neighbors = 0 and count out-calle-neighbors > 0 ] [convierte-en-origen]
  ask nodos with [count out-calle-neighbors = 0 and count in-calle-neighbors > 0] [convierte-en-destino]
end

to modifica-nodos-selec
  ask nodos-seleccionados [
    set color color-nodo
    set densidad densidad-nodo-actual
  ]
end

to oculta-red-completa
  ask nodos [ ht ]
  ask links [ hide-link ]
end

to muestra-red-completa
  ask nodos [ show-turtle ]
  ask links [ show-link ]
end

; operaciones con "selección" (nodos o aristas seleccionados)

to aplica-a-seleccionados
  run (word "selección-" selección)
end

to borra-grafica
  ask nodos [die] ; aristas se borran como consecuencia
end

to-report colores-de-semáforo
  report [semáforo-color-arista] of my-out-calles
end

to nodo-a-semáforo ;;; EDITAR
  ask my-out-calles [set semáforo-color-arista one-of base-colors set es-semáforo-arista? true ]
end

to-report semáforos-aristas
  report calles with [es-semáforo-arista?]
end

to intenta-mostrar-semáforos ; los muestra si y sólo si semáforos-visibles? = true
  ifelse semáforos-visibles? [
    ask semáforos [st]
    ask semáforos-aristas [set color semáforo-color-arista]
  ]
  [
    ask semáforos [ht]
    ask semáforos-aristas [set color color-default-arista]
  ]    
end

to selección-construye-sistema-de-semáforos
   ask nodos-seleccionados [
     nodo-a-semáforo
   ]
end

to nodos-tamaño-pequeño
  set tamaño-nodo .5
  ask nodos [set size tamaño-nodo]
  ask semáforos [set size tamaño-nodo]
end

to nodos-tamaño-mediano
  set tamaño-nodo 1
  ask nodos [set size tamaño-nodo]
  ask semáforos [set size tamaño-nodo]
end

to nodos-tamaño-grande
  set tamaño-nodo 2
  ask nodos [set size tamaño-nodo]
  ask semáforos [set size tamaño-nodo]
end

;to detecta-semáforos
;  deselecciona-toda-arista
;  deselecciona-todo-nodo
;  let s nodos with [not es-semáforo? and count out-calle-neighbors > 1]
;  ask s [ nodo-a-semáforo ]
;  intenta-mostrar-semáforos
;  ask semáforos [selecciona]
;  if (mensajitos?) [show semáforos show semáforos-aristas]
;end

to toggle-muestra-semáforos
  set semáforos-visibles? not semáforos-visibles?
  intenta-mostrar-semáforos
end

to cambia-semáforos
end
    
@#$#@#$#@
GRAPHICS-WINDOW
232
10
671
470
16
16
13.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
0
10
66
43
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
938
24
993
57
NIL
kruskal
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
938
10
997
28
algoritmos
11
0.0
1

INPUTBOX
0
367
101
427
archivo
glorieta.dat
1
0
String

BUTTON
101
400
171
433
guardar
guarda-archivo
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
105
435
181
468
NIL
carga-fondo
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
101
367
171
400
cargar
carga-archivo
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
156
10
223
43
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
688
194
857
227
NIL
modifica-nodos-selec
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
689
10
889
160
Vehiculos en el crucero
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plotxy ticks count vehiculos\nif ticks > 800 [\n  set-plot-x-range ticks - 800 ticks\n]"

SWITCH
137
141
227
174
bi?
bi?
1
1
-1000

BUTTON
150
175
227
208
borra fondo
clear-drawing\nset fondo-actual \"\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
0
272
154
305
NIL
detecta-origenes-destinos
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
761
239
937
272
densidad-nodo-actual
densidad-nodo-actual
0
1
0.09
.01
1
NIL
HORIZONTAL

INPUTBOX
689
289
822
349
nodo-actual-x
10.987750556792871
1
0
Number

INPUTBOX
822
289
978
349
nodo-actual-y
10.767260479064587
1
0
Number

TEXTBOX
803
271
1006
289
coord ultimo nodo seleccionado
11
0.0
1

SWITCH
0
334
171
367
avanza-vehiculos?
avanza-vehiculos?
0
1
-1000

BUTTON
-1
184
111
217
muestra/oculta red
ifelse muestra-red? [\n  if (mensajitos?) [show \"ocultando red...\"]\n  oculta-red-completa\n  set muestra-red? false\n  ][\n  if (mensajitos?) [show \"mostrando red...\"]\n  muestra-red-completa\n  set muestra-red? true\n  ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
134
241
227
274
borra vehiculos
ask vehiculos [die]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
0
43
156
88
acción-actual
acción-actual
"añade-nodo" "borra-nodo" "mueve-nodo" "convierte-en-origen" "convierte-en-destino" "selecciona-nodo" "mueve-nodos-seleccionados" "añade-nodo-a-arista" "------------" "añade-arista" "borra-arista" "selecciona-arista" "invierte-sentido-de-calle" "------------" "añade-semáforo" "mueve-semáforo" "borra-semáforo" "------------" "añade-obstáculo" "mueve-obstáculo" "borra-obstáculo" "------------" "añade-barrera" "mueve-barrera" "borra-barrera"
9

CHOOSER
-1
95
221
140
selección
selección
"todos-los-nodos" "todas-las-aristas" "borra-selección" "intercambia-nodos" "intercambia-aristas" "añade-aristas-entre-nodos" "haz-orígenes" "haz-destinos" "haz-nodos-normales" "construye-sistema-de-semáforos"
5

BUTTON
0
141
134
174
NIL
aplica-a-seleccionados
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
698
351
898
501
Velocidad promedio
NIL
NIL
0.0
10.0
0.0
1.5
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? vehiculos [\n plotxy ticks mean [velocidad] of vehiculos\n]\nif ticks > 800 [\n  set-plot-x-range ticks - 800 ticks\n]"

INPUTBOX
0
435
105
508
fondo
http://maps.googleapis.com/maps/api/staticmap?center=19.3336169,-99.1514492&zoom=18&size=429x429&maptype=satellite&sensor=true\n
1
1
String

BUTTON
145
208
227
241
NIL
borra-grafica\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
901
403
1021
436
NIL
clear-all-plots
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
882
198
1036
231
muestra/oculta semáforos
toggle-muestra-semáforos
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
689
228
762
288
color-nodo
48
1
0
Color

BUTTON
892
61
1037
94
NIL
nodos-tamaño-pequeño
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
892
93
1037
126
NIL
nodos-tamaño-mediano
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
892
126
1037
159
NIL
nodos-tamaño-grande
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## ¿QUÉ ES?

Programa escrito en el lenguaje Netlogo (descargable de http://ccl.northwestern.edu/netlogo) para simular las condiciones de tráfico vehicular en un crucero, mediante un modelo basado en agentes computacionales.

El programa muestra el tráfico formado por vehículos simulados mediante "caminates aleatorios" sobre una red que se superpone al mapa de un crucero de la Ciudad de México

## COMO FUNCIONA

Una vez descargado, recortado un rectángulo de un mapa de la Ciudad (opcionalmente se limpia de etiquetas y símbolos), se procede a trazar los nodos y aristas (dirigidas) por donde circularán las calles, indicando el sentido de las mismas. A continuación se colorean los nodos "origen" (rojo) y "destino" (verde), seleccionándolos y luego modificando sus características, incluyendo la frecuencia con la que los vehículos son generados de los orígenes (densidad).

## COMO USARLO

Una vez descargado e instalado el software Netlogo (gratuito), se abre el programa y se
presiona el botón "cargar", aceptando el nombre de archivo default "carriles.dat". Este
a su vez indica que se cargue tanto el mapa "carriles.png" como los datos de la gráfica
en "carriles.dat". A continuación se presiona "go", pudiendo presionar "muestra/oculta
red".

## COSAS PARA NOTAR

Los nodos pueden moverse individualmente o en grupo (seleccionándolos), mediante la opción correspondiente, así como borrarse. Las aristas se pueden trazar entre nodos, cambiarles su sentido etc. Para crear una calle de doble sentido pueden trazarse dos aristas.

Los vehículos no avanzarán si tienen un vehículo enfrente. Esto puede provocar congestionamientos al quedar bloqueados o simplemente puede provocar un avance más lento.

## COSAS PARA PROBAR

Utiliza otros mapas y créale sus propias calles, altera los sentidos de las calles, diseña puentes vehiculares y vé como aumenta o disminuye la velocidad promedio y número de vehículos en el crucero, etc.

## EXTENDIENDO EL MODELO

En futuras versiones se añadirán características que disminuyen o regulan el tráfico como semáforos, topes y puntos conflictivos (como escuelas, estaciones de servicio, paradas de transporte público, etc.)

## CARACTERÍSTICAS DE NETLOGO

El programa usa de manera intensiva el ratón, y dado que Netlogo es muy pobre en este aspecto, el modelo resulta algo limitado. Aún así las diferentes formas de agregar, cambiar o borrar objetos con el ratón se trató de hacer lo mas intuitiva y parecida a los sistemas de dibujo tradicionales.

## MODELOS RELACIONADOS

Aquellos modelos vehiculares y de gráficas.

## CREDITOS Y REFERENCIAS

(C) 2014 Felipe Humberto Contreras Alcalá, Oscar Valdéz Ambrosio
Este programa fué realizado como parte del proyecto de investigación PI2011-56 de la Universidad Autónoma de la Ciudad de México con apoyo y agradecimientos a la Secretaría del Ciencia, Tecnología e Innovación (SECITI) del Distrito Federal, México.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

car top
true
0
Polygon -7500403 true true 151 8 119 10 98 25 86 48 82 225 90 270 105 289 150 294 195 291 210 270 219 225 214 47 201 24 181 11
Polygon -16777216 true false 210 195 195 210 195 135 210 105
Polygon -16777216 true false 105 255 120 270 180 270 195 255 195 225 105 225
Polygon -16777216 true false 90 195 105 210 105 135 90 105
Polygon -1 true false 205 29 180 30 181 11
Line -7500403 false 210 165 195 165
Line -7500403 false 90 165 105 165
Polygon -16777216 true false 121 135 180 134 204 97 182 89 153 85 120 89 98 97
Line -16777216 false 210 90 195 30
Line -16777216 false 90 90 105 30
Polygon -1 true false 95 29 120 30 119 11

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

emblem2
false
0
Polygon -7500403 true true 0 90 15 120 285 120 300 90
Polygon -7500403 true true 30 135 45 165 255 165 270 135
Polygon -7500403 true true 60 180 75 210 225 210 240 180
Polygon -7500403 true true 150 285 15 45 285 45
Polygon -16777216 true false 75 75 150 210 225 75
Polygon -1184463 true false 75 75 225 75 150 210 75 75

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.5
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

dos carriles
0.0
-0.2 1 1.0 0.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

selec
0.0
-0.2 0 0.0 1.0
0.0 1 2.0 2.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

tres carriles
0.0
-0.2 1 1.0 0.0
0.0 1 1.0 0.0
0.2 1 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
