;;;; Grupo 13 Goncalo Santos 78093 Manuel Santos 78445 Beatriz Portugal 79175

;;; --- TIPO ACCAO ---

(defun cria-accao (int arr)
  (cons int arr))

(defun accao-coluna (accao)
  (car accao))

(defun accao-peca (accao)
  (cdr accao))


;;; --- TIPO TABULEIRO ---

(defun cria-tabuleiro ()
  (make-array '(18 10)))

(defun copia-tabuleiro (tab)
  (let ((copy-tab (make-array '(18 10))))
    (loop for i from 0 to 17
        do (loop for j from 0 to 9
               do (setf (aref copy-tab i j) (aref tab i j))))
    copy-tab))

(defun tabuleiro-preenchido-p (tab i j)
  (aref tab i j))

(defun tabuleiro-altura-coluna (tab j)
  (loop for i from 17 downto 0
      do (if (aref tab i j)
             (return-from tabuleiro-altura-coluna (1+ i))))
  0)

(defun tabuleiro-linha-completa-p (tab i)
  (loop for j from 0 to 9
      do (if (null (aref tab i j))
             (return-from tabuleiro-linha-completa-p nil)))
  t)

(defun tabuleiro-preenche! (tab i j)
  (if (and (> i -1) (< i 18) (> j -1) (< j 10))
      (setf (aref tab i j) t)))

(defun tabuleiro-remove-linha! (tab linha)
  (loop for i from linha to 17
      do (loop for j from 0 to 9
             do (if (= i 17)
                    (setf (aref tab i j) nil)
                  (setf (aref tab i j) (aref tab (1+ i) j))))))

(defun tabuleiro-topo-preenchido-p (tab)
  (loop for j from 0 to 9
      do (if (aref tab 17 j)
             (return-from tabuleiro-topo-preenchido-p t)))
  nil)

(defun tabuleiros-iguais-p (tab1 tab2)
  (if (equalp tab1 tab2)
      (loop for i from 0 to 17
          do (loop for j from 0 to 9
                 do (if (not (eq (aref tab1 i j) (aref tab2 i j)))
                       (return-from tabuleiros-iguais-p nil))))
    (return-from tabuleiros-iguais-p nil))
  t)

(defun tabuleiro->array (tab)
  (copia-tabuleiro tab))

(defun array->tabuleiro (arr)
  (copia-tabuleiro arr))


;;; --- TIPO ESTADO ---

(defstruct estado
  pontos
  pecas-por-colocar
  pecas-colocadas
  Tabuleiro)
  
(defun copia-estado (estado)
  (let ((estado-novo (make-estado)))
    (setf (estado-pontos estado-novo) (estado-pontos estado))
    (setf (estado-pecas-por-colocar estado-novo) (copy-list (estado-pecas-por-colocar estado)))
    (setf (estado-pecas-colocadas estado-novo) (copy-list (estado-pecas-colocadas estado)))
    (setf (estado-Tabuleiro estado-novo) (copia-tabuleiro (estado-Tabuleiro estado)))
    estado-novo))

(defun estados-iguais-p (estado1 estado2)
  (equalp estado1 estado2))

(defun estado-final-p (estado)
  (or (tabuleiro-topo-preenchido-p (estado-Tabuleiro estado)) (eq (estado-pecas-por-colocar estado) nil)))


;;; --- TIPO PROBLEMA ---
  
(defstruct problema
  estado-inicial
  solucao
  accoes
  resultado
  custo-caminho)


;;; --- FUNCOES DO PROBLEMA DE PROCURA ---

(defun solucao (estado)
  (and (not (tabuleiro-topo-preenchido-p (estado-Tabuleiro estado))) (eq (estado-pecas-por-colocar estado) nil)))

(defun accoes (estado)
  (let ((lista (make-list 0)) 
        (peca-actual (car (estado-pecas-por-colocar estado))))
    (if (estado-final-p estado)
        (return-from accoes nil)
      (progn
        (loop for x from 0 to (aux-num-rotacoes peca-actual)
            do (loop for y to (- 10 (array-dimension (eval (intern (string-upcase (concatenate 'string "peca-" (write-to-string peca-actual) (write-to-string x))))) 1))
                   do (setf lista (append lista (make-list 1 :initial-element (cria-accao y (eval(intern(string-upcase(concatenate 'string "peca-" (write-to-string peca-actual) (write-to-string x)))))))))))
        lista))))

(defun aux-num-rotacoes (peca)
  (case peca
    (i '1)
    (l '3)
    (j '3)
    (o '0)
    (s '1)
    (z '1)
    (t '3)))

(defun resultado (estado accao)
  (let ((estado-novo (copia-estado estado))
        (num-linhas-completas))
    
    (setf num-linhas-completas 0)
    (setf (estado-pecas-colocadas estado-novo) (cons (car (estado-pecas-por-colocar estado)) (estado-pecas-colocadas estado-novo)))
    (setf (estado-pecas-por-colocar estado-novo) (cdr (estado-pecas-por-colocar estado-novo)))
    
    (loop for x from 0 to (1- (array-dimension (cdr accao) 0))
        do (loop for y from 0 to (1- (array-dimension (cdr accao) 1))
               do (if (aref (cdr accao) x y)
                      (tabuleiro-preenche! (estado-Tabuleiro estado-novo) (+ (tabuleiro-linha estado accao) x) (+ (car accao) y)))))
    
    (if (tabuleiro-topo-preenchido-p (estado-Tabuleiro estado-novo))
        (return-from resultado estado-novo)
      (progn
        (loop for i from 17 downto 0
            do (if (tabuleiro-linha-completa-p (estado-Tabuleiro estado-novo) i)
                   (progn
                     (setf num-linhas-completas (1+ num-linhas-completas))
                     (tabuleiro-remove-linha! (estado-Tabuleiro estado-novo) i)
                     (setf (estado-pontos estado-novo) (+ (pontuacao num-linhas-completas) (estado-pontos estado))))))))
    (return-from resultado estado-novo)))
        
(defun pontuacao (int)
  (case int
    (0 '0)
    (1 '100)
    (2 '300)
    (3' 500)
    (4' 800)))

(defun tabuleiro-linha (estado accao)
  (let ((altura-max)
        (altura-peca))
    (setf altura-max 0)
    (setf altura-peca 0)
    (loop for x from 0 to (1- (array-dimension (cdr accao) 0))
        do (loop for y from 0 to (1- (array-dimension (cdr accao) 1))
               do (if (aref (cdr accao) x y)
                      (if (> (tabuleiro-altura-coluna (estado-Tabuleiro estado) (+ (car accao) y)) (+ altura-max altura-peca))
                          (setf altura-max (- (tabuleiro-altura-coluna (estado-Tabuleiro estado) (+ (car accao) y)) altura-peca)))))
          (setf altura-peca (1+ altura-peca)))
    (return-from tabuleiro-linha altura-max)))

(defun qualidade (estado)
  (* (estado-pontos estado) -1))

(defun custo-oportunidade (estado)
  (let
    ((max-res 0) 
    (pecas (estado-pecas-colocadas estado)) 
    (pontos (estado-pontos estado)))
    (loop for i from 0 to (1- (list-length pecas))
        do (setf max-res (+ (aux-calc-pontuacao (car pecas)) max-res))
          (setf pecas (cdr pecas)))
    (- max-res pontos)))

(defun aux-calc-pontuacao (peca)              
  (case peca
    (i '800)
    (l '500)
    (j '500)
    (o '300)
    (s '300)
    (z '300)
    (t '300)))

;;; Funcoes de Procura

;;; Procura em Profundidade Primeiro

;;; procura-pp: problema -> lista
;;; retorna uma lista de accoes utilizando a procura em profundidade primeiro

(defun procura-pp (p)
  
  (let ((lista-abertos (make-list 0))
        (lista-accoes (make-list 0))
        (no-aux (make-node))
        (sucessor nil))
    
    (setf (node-estado no-aux) (problema-estado-inicial p))
    (push no-aux lista-abertos)
    
    (loop while (not (null lista-abertos)) do
      
      (setf no-aux (pop lista-abertos))
      (if (funcall (problema-solucao p) (node-estado no-aux))
          
          (return-from procura-pp (res-pp no-aux))
        
        (progn
          
          (setf lista-accoes (funcall (problema-accoes p) (node-estado no-aux)))
          
          (loop while (not (null lista-accoes)) do    
            
            (setf sucessor (make-node))
            (setf (node-p-node sucessor) no-aux)
            (setf (node-accao sucessor) (car lista-accoes))
            (setf (node-estado sucessor) (funcall (problema-resultado p) (node-estado no-aux) (car lista-accoes)))
            (setf lista-accoes (cdr lista-accoes))
            
            (push sucessor lista-abertos)))))
    
    (return-from procura-pp nil)))

;;; estrutura com informacoes de cada no utilizada na procura profundidade primeiro

(defstruct node
  estado             ; estado atual para cada no
  accao              ; accao aplicada ao estado
  p-node)            ; no pai

;;; res-pp: no -> lista
;;; retorna uma lista com o resultado da aplicacao da funcao procura-pp

(defun res-pp (n)
  (let ((res (make-list 0)))
    (loop while (not (eq (node-p-node n) nil)) do
      (setf res (cons (node-accao n) res))
      (setf n (node-p-node n)))
    (return-from res-pp res)))


;;; procura-A* - funcoes e estruturas 
;;; estrutura com informacoes de cada no utilizada na procura A*

(defstruct no
  estado             ; estado atual para cada no
  caminho            ; lista de acoes desde o estado inicial ate o no corrente
  custo)             ; soma do custo desde o no inicial com a heuristica

;;; procura-A* : problema x heuristica -> lista
;;; devolve a lista de accoes de modo a maximizar os pontos obtidos

(defun procura-A* (p h)
  (let ((open-list (list))
        (novo-no)
        (current-no))
    
    (push (make-no :estado (problema-estado-inicial p) :caminho nil :custo 0) open-list)
    
    (loop do
          (if (null open-list)
              (return-from procura-A* nil))

          (setf current-no (pop open-list))
          (if (funcall(problema-solucao p) (no-estado current-no))
          (return-from procura-A* (no-caminho current-no)))
          
                   
      (loop for x in (funcall(problema-accoes p) (no-estado current-no)) do
            (progn
              
              (setf novo-no (make-no :estado (funcall (problema-resultado p) (no-estado current-no) x)
                                     :caminho (append (no-caminho current-no) (list x))
                                     :custo (+ (funcall h (funcall (problema-resultado p) (no-estado current-no) x))
                                               (funcall (problema-custo-caminho p) (funcall (problema-resultado p) (no-estado current-no) x)))))
             
              (setf open-list (insert-no open-list novo-no)))))))
              

;;; insert-no: lista x no -> lista
;;; funcao que recebe uma lista de nos e um no, insere o no na lista e devolve a lista com os nos ordenados por valor de custo

(defun insert-no (lista no)
  (cond ((null lista) (list no))
        ((<= (no-custo no) (no-custo (car lista))) (append (list no) lista))
        (t (append (list (car lista)) (insert-no (rest lista) no)))))


;;; procura-best
;;; utiliza como heuristica a funcao Qualidade

(defun procura-best (array lista-pecas)
  (let ((problema)
        (estado)
        (heuristica))
    (setf estado (make-estado :pontos 0 :pecas-por-colocar lista-pecas :pecas-colocadas nil :tabuleiro array))
    (setf problema (make-problema :estado-inicial estado :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'custo-oportunidade))
    (setf heuristica #'qualidade)
    (return-from procura-best (procura-A* problema heuristica))))


;;; funcoes heuristicas

;;; heuristica-altura-total: estado -> inteiro
;;; heuristica que calcula a altura total das colunas do jogo. Pretende-se minimizar este valor

(defun heuristica-altura-total (estado)
  (let ((resultado 0))
    (loop for x from 0 to 9 do
          (setf resultado (+ resultado (* 30 (tabuleiro-altura-coluna (estado-Tabuleiro estado) x)))))
    (return-from heuristica-altura-total resultado)))


;;; heuristica-buracos: estado -> inteiro
;;; heuristica que calcula o numero de buracos existentes no jogo. Pretende-se minimizar este valor

(defun heuristica-buracos (estado)
  (let ((resultado 0))
    (loop for i from 0 to 16
      do (loop for j from 0 to 9
               do(if (and (not (aref (estado-Tabuleiro estado) i j)) (aref (estado-Tabuleiro estado) (1+ i) j))
                     (setf resultado (* 500 (1+ resultado))))))
    (return-from heuristica-buracos resultado)))

           
(load "utils.fas")