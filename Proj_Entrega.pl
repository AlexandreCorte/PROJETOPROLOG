%TRABALHO REALIZADO POR ALEXANDRE CORTE, 99048, TG.
%---------------------------------------------------------------------

:-[codigo_comum].

/* Recebe com input uma lista e devolve a soma de todos os elemntos 
dessa mesma lista*/
soma_lista([], 0).
soma_lista([Head|Tail], X) :- 
    soma_lista(Tail, X1), 
    X is X1+Head.

/*Predicado 1: Recebe como input um inteiro N, uma lista de elementos
Els, um inteiro Soma e devolve Combs, que sao todas as combinacoes possiveis,
em que uma combinacao e uma lista tal que a soma dos seus N
elementos e igual a variavel Soma passada com input.*/
combinacoes_soma(N, Els, Soma, Combs):-
    findall(C, (combinacao(N,Els,C), soma_lista(C, X), X=:=Soma), Combs).

/*Predicado 2: Como o nome indica, permuta as combinacoes do predicado
anterior, ou seja, altera a ordem dos numeros pertecentes a combinacao e devolve 
uma lista com todas as possibilidades incluidas.*/
permutacoes_soma(N,Els,Soma,Perms):-
    combinacoes_soma(N,Els,Soma,P),
    findall(Perm, (member(X,P), permutation(X, Perm)), Permutacoes),
    sort(Permutacoes,Perms).

/*Funcao auxiliar do predicado Espaco Fila: Recebe um inteiro El1, um inteiro
El2 e a Fila passada no predicado espaco_fila e retorna uma lista com um conjunto
de variaveis livres, que pertencem a fila e se encontram entre
os indices El1 e El2 que sao passados.*/
add_underscores(El1,El2,Fila,Underscores):-
add_underscores_aux(El1,El2,Fila,Underscores,Res),
Underscores=Res.

add_underscores_aux(El1,El2,Fila,Underscores,[Elemento|Res]):-
    El1=<El2,!,
    El1_Mais_1 is El1+1,
    nth1(El1, Fila, Elemento),
    add_underscores_aux(El1_Mais_1,El2,Fila, Underscores, Res).
    
add_underscores_aux(_,_,_,_,[]).

/*Funcao auxiliar que recebe uma string H_V, uma Fila, uma Lista de Indices e 
devolve uma lista Val, que, consoante a letrapassada no input e H ou V, preenche 
a lista Val com as posicoes de todos os espacos da Fila*/ 
h_ou_v(H_V,Fila,Val, Lista_indices):-
    (H_V=='h'->findall(El, (member(Y,Lista_indices), Y_Mais_1 is Y+1,
    nth1(Y, Fila, X), nth1(Y_Mais_1, Fila, Z), var(Z), is_list(X),
    nth1(2,X,El)), Val)
    ; 
    findall(Elemento, (member(A,Lista_indices), A_Mais_1 is A+1,
    nth1(A, Fila, B), nth1(A_Mais_1, Fila, C), var(C), is_list(B),
    nth1(1,B,Elemento)), Val)).

/*Predicado 3: Esta e a pior funcao do meu projeto, mas confesso que depois de a 
ter terminado nao tive coragem de voltar a tocar-lhe ate ao final. Recebe uma Fila, uma 
string H_V e devolve Esp, tal que Esp e, em cada iteracao, um espaco presente na fila, 
em que espaco e da forma espaco(X,Y), sendo X o numero do espaco e Y as variaveis livres 
a ele associadas.*/
espaco_fila(Fila,Esp,H_V):-
    length(Fila,Tamanho_Fila),
    Tamanho_Fila_Menos_1 is Tamanho_Fila-1,
    numlist(1,Tamanho_Fila_Menos_1,Lista_indices), 
    /*lista de indices, que vai de 1 ate ao tamanho da fila-1, visto que o ultimo elemento e irrelevante*/
    h_ou_v(H_V,Fila,Val, Lista_indices),
    /*Val e uma lista com as posicoes dos espacos*/
    length(Val,Tamanho_Val),
    length(Fila,Tamanho),
    Tamanho_Mais_1 is Tamanho+1,
    findall(Pos, (between(1,Tamanho,Pos), nth1(Pos,Fila,X), is_list(X)), Poslista),
    append(Poslista, [Tamanho_Mais_1], Poslistafinal),
    /*PosListaFinal e uma lista com todas as posicoes das listas presentes na Fila. No final e adicionado
    a lista o tamanho da Fila, visto que este e necessario para fazer o intervalo em que se encontram 
    as variaveis livres na Fila*/
    length(Poslistafinal, Dimensao),
    Dimensao_Menos_1 is Dimensao-1,
    numlist(1,Dimensao_Menos_1,Indices),
    bagof(Lista, espaco_fila_1(Fila,Indices,Poslistafinal, Lista), Under),
    numlist(1,Tamanho_Val,Ind_Val),
    Esp=espaco(X,Und), member(A, Ind_Val), nth1(A,Val,X), nth1(A,Under,Under1), flatten(Under1,Und).

/*Funcao auxiliar que recebe a Fila, uma lista com indices de todas as listas presentes 
em Fila e uma lista com os numeros de 1 ate ao tamanho da lista anterior. Aplica o bagof
e guarda uma lista Under que contem variaslistas com todas as variaveis livres devidamente
separadas nos respetivos espacos. Para isso soma 1 unidade a posicao da primeira lista,
subtrai uma unidade a posicao da segunda lista e vai buscar todas as variaveis livres
que se encontram entre estas duas posicoes. Depois continua a percorrer a lista com as 
posicoes das listas ate estaterminar*/
espaco_fila_1(Fila,Indices,Poslistafinal, Under):-
    bagof(Underscores, (member(N, Indices), N_Mais_1 is N+1, nth1(N, Poslistafinal, El1), 
            nth1(N_Mais_1, Poslistafinal, El2), El_1 is El1+1, El_2 is El2-1, El_1=<El_2,
            add_underscores(El_1,El_2,Fila, Underscores)), Under).

/*Predicado 4: Aplica o bagof no terceiro predicado, criando assim uma lista com todos
os espacos presentes na fila*/  
espacos_fila(H_V,Fila,Esps):-
    bagof(Esp, espaco_fila(Fila,Esp,H_V), Esps),!.
espacos_fila(_,_,[]).

/*Funcao auxiliar do predicado 11. Esta funcao recebe um Elemento, que e o primeiro elemento
de uma certa lista passada no Lst_Perms, recebe o tamanho dessa lista e cria uma lista com 
length Tamanho em que os seus elementos sao todos iguais a Elemento.
A ideia deste predicado e posteriormente aplicar a transposta no Lst_Perms e verificar
se esta Lista_Num_Iguais e Igual a transposta da Lst_Perms.*/
cria_lista_igual(Elemento,Tamanho,Lista_Num_Iguais):-
cria_lista_igual_aux(Elemento,Tamanho,Lista_Num_Iguais,Res),
Lista_Num_Iguais=Res.

cria_lista_igual_aux(Elemento,Tamanho,Lista_Num_Iguais,[Elemento|Res]):-
    Tamanho>0,!,
    Tamanho_Menos_1 is Tamanho-1,
    cria_lista_igual_aux(Elemento,Tamanho_Menos_1,Lista_Num_Iguais,Res).
cria_lista_igual_aux(_,_,_,[]).

/*Predicado 11: visto que enquanto tentava fazer o 3 e ja em desespero total fui tentando
fazer outros predicados que nao dependessem do mesmo.
Recebe uma lista que pode ou nao conter varias outras listas e depois Numeros Comuns, que
e uma lista com todos os numeros iguais que se encontram na mesma posicao nas diversas listas
de Lst_Perms. Numeros comuns e uma lista de elementos do tipo (X,Y), em que X corresponde
a posicao do elemento comum e Y e verdadeiramente o elemento comum*/
numeros_comuns(Lst_Perms, Numeros_comuns):-
    mat_transposta(Lst_Perms,Transposta),
    findall(El, (member(X,Transposta), nth1(1,X,El)), Lista_First_Number),
    findall(Tamanho, (member(X,Transposta), length(X,Tamanho)), Lista_Tamanhos),
    findall(Lista_Num_Iguais, (member(Y, Lista_First_Number), nth1(1, Lista_Tamanhos,Z), 
    cria_lista_igual(Y,Z,Lista_Num_Iguais)), Listas_Iguais),
    length(Listas_Iguais,Tamanho_Listas_Iguais),
    findall((Pos,El), (between(1,Tamanho_Listas_Iguais,Pos), nth1(Pos, Listas_Iguais, Lista1), 
    nth1(Pos,Transposta, Lista2), nth1(1,Lista1,El), Lista1=Lista2),
    Numeros_comuns).

/*Predicado 5: Recebe um Puzzle Kakuro e retorna Espacos, tal que Espacos e uma lista com 
todos os espacos horizontais e verticais do Puzzle*/
espacos_puzzle(Puzzle,Espacos):-
    mat_transposta(Puzzle,Puzzle_Transposto),
    bagof(Espacos_Inicial, junta_puzzle(Puzzle, Espacos_Inicial), Esp_Ini),
    bagof(Espacos_Transp, junta_puzzle_transposto(Puzzle_Transposto,Espacos_Transp), Esp_Fin),
    append(Esp_Ini,Esp_Fin,Espacos).

/*Funcao auxiliar para aplicar Member no Puzzle para aceder a cada um dos espacos
horizontais do mesmo*/
junta_puzzle(Puzzle,Esp):-
    member(Fila_Puzzle,Puzzle), 
    espaco_fila(Fila_Puzzle,Esp,h).

/*Funcao auxiliar para aplicar Member no Puzzle Transposto para aceder a cada um
dos espacos verticais do Puzzle Original*/
junta_puzzle_transposto(Puzzle,Esps):-
    member(Fila_do_Puzzle,Puzzle), 
    espaco_fila(Fila_do_Puzzle,Esps,v).

/*Predicado 7: Recebe uma lista de Espacos e retorna uma lista com elementos 
espaco(X,Y), em que X e o numero do espaco e Y e a lista com todas as permutacoes
possiveis cuja soma e igual a soma do espaco.*/
permutacoes_soma_espacos(Espacos,Perms_Soma):-
    bagof(Perms, permutacoes_soma_espacos_aux(Espacos,Perms), Perms_Soma).

/*Funcao auxiliar do predicado 7 que devolve, membro a membro, uma lista com 
um espaco e as permutacoes possiveis do mesmo*/
permutacoes_soma_espacos_aux(Espacos,Lista_Perms):-
    member(espaco(X,Y), Espacos),
    length(Y, Tamanho), 
    permutacoes_soma(Tamanho, [1,2,3,4,5,6,7,8,9], X, Z),
    Lista_Perms=[espaco(X,Y), Z].

/*Predicado 6: Recebe uma lista de espaco, um Esp que e um espaco em concreto e 
devolve uma lista com todos os espacos que tem posicoes comuns com Esp*/
espacos_com_posicoes_comuns(Espacos,Esp,Esps_Com):-
    exclude(==(Esp), Espacos, Esps_Novo), 
    /*Excluir o proprio Esp, visto que este nao e pretendido no resultado final*/
    bagof(Esps, espacos_com_posicoes_comuns_aux(Esps_Novo,Esp,Esps), Esps1),
    flatten(Esps1,Esps_Com).

/*Funcao auxiliar do predicado 6, que recebe duas listas Vars_Espaco, com as
variaveis livres de um espaco da lista de espacos e Vars_Esp com as variaveis
livres de Esp e retorna True no caso de pelo menos uma variavel livre do Espaco
 estar contida em Esp*/
intersects(Vars_Espaco,Vars_Esp):-
    member(X,Vars_Espaco),
    contains_var(X,Vars_Esp).

/*Funcao auxiliar do predicado 6, que agrega todos os espacos que devolvem
True na funcao Intersects,que verifica se 2 listas tem alguma variavel livre em comum.*/
espacos_com_posicoes_comuns_aux(Espacos,espaco(_,B),Esps):-
    bagof(espaco(X,Y), (member(espaco(X,Y), Espacos), intersects(Y,B)), Esps). 

/*Funcao auxiliar do predicado 8, que devolve 1 se for possivel unificar a
lista Y, que corresponde a uma permutacao do espaco com alguma permutacao
presente na lista Permutacao, ou devolve 0 caso contrario*/
verifica_permutacao(espaco(_,Y), Permutacao, Res):-
    member(P, Permutacao),
    subsumes_term(Y, P) -> Res is 1
    ;
    Res is 0.

/*Funcao auxiliar do predicado 8, que aplica o forall a lista final e devolve
True se todas as listas tiverem resultado 1, ou seja, devolve True, caso em
todas as listas de permutacoes haja pelo menos 1 permutacao que possa ser 
unificada com a permutacao do espaco*/
permutacao_possivel_espaco_aux(El, Permutacoes_Possiveis, El):-
    bagof(espaco(X,Y), permutacao_possivel_espaco_aux_1(Permutacoes_Possiveis, espaco(X,Y)),
    Permutacoes_Finais),
    bagof(Lista_Permutacoes, 
    permutacao_possivel_espaco_aux_2(Permutacoes_Possiveis, Lista_Permutacoes),
    Lista_Perms_Final),
    length(Permutacoes_Finais,Tamanho_Perms),
    numlist(1,Tamanho_Perms, Lista_Indices),
    bagof(Lista_Final, 
    permutacao_possivel_aux_3(Lista_Indices, Permutacoes_Finais, Lista_Perms_Final, Lista_Final),
    Lista_Final_1),
    forall(member(X, Lista_Final_1), X==[1]).

permutacao_possivel_aux_3(Lista_Indices, Permutacoes_Finais, Lista_Perms_Final, Resultado):- 
    member(N, Lista_Indices), 
    nth1(N, Permutacoes_Finais, Espaco), 
    nth1(N, Lista_Perms_Final, Permutacao),
    bagof(Res, verifica_permutacao(Espaco, Permutacao, Res), Resultado).

/*Funcao auxiliar do predicado 8. Utilizada para conseguir todas as permutacoes*/
permutacao_possivel_espaco_aux_2(Permutacoes_Possiveis, Lista_Permutacoes):-
    member(Perm_Poss, Permutacoes_Possiveis),
    nth1(2, Perm_Poss, Lista_Permutacoes).

/*Funcao auxiliar do predicado 8. Utilizada para conseguir todos os espacos*/
permutacao_possivel_espaco_aux_1(Permutacoes_Possiveis, espaco(X,Y)):-
    member(Perm_Poss, Permutacoes_Possiveis),
    nth1(1, Perm_Poss, espaco(X,Y)).

/*Predicado 8: Recebe uma lista de espacos, uma lista de permutacoes, um espaco
e retorna todas as Permutacoes, em que uma Permutacao so e possivel caso exista
pelo menos uma permutacao de cada espaco comum tal que a sua unificacao e possivel
com o espaco*/
permutacao_possivel_espaco(Permutacoes_Finais,espaco(X,Y),Espacos,Perms_Soma):-
    Perms_Soma=Perms_Soma,
    espacos_com_posicoes_comuns(Espacos,espaco(X,Y),Esps_Com),
    permutacoes_soma_espacos(Esps_Com, Permutacoes_Possiveis),
    permutacoes_soma_espacos([espaco(X,Y)], Esp_Perm),
    nth1(1, Esp_Perm, Perms_Possiveis_Espaco),
    nth1(2, Perms_Possiveis_Espaco, Permutacoes),
    member(Permutacao,Permutacoes),
    Y=Permutacao,
    permutacao_possivel_espaco_aux(Y, Permutacoes_Possiveis, Permutacoes_Finais). 

/*Predicado 9: Igual ao predicado anterior, mas ao inves de retornar as posicoes 
uma a uma, cria uma lista com todas as permutacoes possiveis para um dado Espaco*/
permutacoes_possiveis_espaco(Espacos,Perms_Soma,espaco(X,Y),Perms_Poss):-
    findall(Perms_Possiveis, permutacao_possivel_espaco(Perms_Possiveis, 
    espaco(X,Y), Espacos, Perms_Soma), Perms_Possiveis_Total),
    Perms_Poss=[Y, Perms_Possiveis_Total].

/*Funcao auxiliar do predicado 9, que aplica member ao permutacoes_soma_espacos
para depois ser aplicado o bagof e criar uma lista com todas as permutacoes possiveis*/
permutacoes_possiveis_espaco_aux(Espacos,Perms_Poss_Esps):-
    permutacoes_soma_espacos(Espacos,Perms_Soma),
    member(Esp, Espacos), 
    permutacoes_possiveis_espaco(Espacos, Perms_Soma, Esp, Perms_Poss_Esps).

/*Predicado 10: Semelhante aos 2 predicados anteriores, mas este retorna as 
permutacoes possiveis de todo o Puzzle.*/
permutacoes_possiveis_espacos(Espacos, Permutacoes):-
        bagof(Perms_Poss_Esps, permutacoes_possiveis_espaco_aux(Espacos,Perms_Poss_Esps), Permutacoes).

/*Funcao auxiliar do predicado 12, cuja unica utilidade e proceder a unificacao de cada numero 
comum das permutacoes com o espaco*/
atribui_comuns_aux([Head|Tail],[Head2|Tail2], Vars):-
    nth1(Head,Vars,El),
    El=Head2,
    atribui_comuns_aux(Tail,Tail2, Vars),!.
atribui_comuns_aux([],_,_).
atribui_comuns_aux(_,[],_).

/*Predicado 12: Recebe as Permutacoes Possiveis e retorna as Permutacoes Possiveis 
atualizadas, sendo que esta atualizacao passa por verificar se existem numeros comuns
nas permutacoes e fazer a sua unificacao com cada espaco*/
atribui_comuns(Perms_Possiveis):-
aux_atribui_comuns(Perms_Possiveis).

/*Funcao auxiliar do predicado 12 que recebe as Permutacoes Possiveis, acede ao espaco
e as permutacoes,aplica o numeros comuns nas permutacoes, guarda as posicoes e elementos
dos numeros comuns e chama outra funcao auxiliar para unificar cada um dos elementos 
pretendidos.*/
aux_atribui_comuns([]):-!.
aux_atribui_comuns([Head|Tail]):-
    nth1(1, Head, Vars),
    nth1(2, Head, Nums),
    numeros_comuns(Nums,Nums_Comuns),
    findall(Pos, member((Pos, El), Nums_Comuns), Posicoes),
    findall(El, member((Pos, El), Nums_Comuns), Elementos),
    atribui_comuns_aux(Posicoes,Elementos,Vars),
    aux_atribui_comuns(Tail),!.

/*Funcao auxiliar do predicado 13 que serve para verificar quais sao as permutacoes que podem
ser unificadas com o espaco*/
verifica_unificacao(Vars,Perms,Resultado):-
    findall(Permutacao, (member(Permutacao,Perms), subsumes_term(Vars,Permutacao)), Resultado).

/*Predicado 13: Este predicado recebe uma lista de Permutacoes Possiveis e retorna uma 
lista de Novas Permutacoes Possiveis que resulta de eliminar as Permutacoes que se 
tornaram impossiveis apos aplicar o predicado anterior. Por exemplo, nao faz sentido 
existir um espaco da forma [2,P1,P2] e uma das suas permutacoes possiveis ser [1,2,3],
por isso esta deve ser eliminada*/
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis):-
retira_impossiveis_aux(Perms_Possiveis, Novas_Perms_Possiveis, Res),
Novas_Perms_Possiveis=Res.

/*Funcao auxiliar do predicado 13 que serve para ir recursivamente construindo as novas
permutacoes possiveis com as permutacoes que podem ser unificadas com o espaco*/
retira_impossiveis_aux([Head|Tail], Novas_Perms_Possiveis, [[Vars, Resultado]|Res]):-
    nth1(1, Head, Vars),
    nth1(2, Head, Perms),
    verifica_unificacao(Vars,Perms,Resultado),
    retira_impossiveis_aux(Tail, Novas_Perms_Possiveis, Res),!.
retira_impossiveis_aux(_,_,[]).
retira_impossiveis_aux([],_,_).

/*Predicado 14: Recebe uma lista de Permutacoes Possiveis e devolve uma lista com as 
novas permutacoes possiveis, que resultam de aplicar o atribui comuns e o retira 
impossives as permutacoes possiveis ate a lista de permutacoes ficar inalterada.
Este algoritmo e aplicado visto que podem existir alteracoes nas permutacoes depois
de aplicarmos o retira impossiveis e podemos passar a ter novamente numeros comuns
em todas as permutacoes*/
simplifica(Perms_Possiveis,Novas_Perms_Possiveis):-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis,Permutacoes),
    Permutacoes\=Perms_Possiveis, !,
    simplifica(Permutacoes,Novas_Perms_Possiveis);
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis,Permutacoes),
    Novas_Perms_Possiveis=Permutacoes.

/*Predicado 15: Recebe um Puzzle de jogo e retorna as Permutacoes Possiveis simplificadas 
desse Puzzle. Alguns Puzzles podem ficar imediatamente solucionados apos aplicar este
predicado*/
inicializa(Puzzle,Perms_Possiveis):-
    espacos_puzzle(Puzzle,Espacos),
    permutacoes_possiveis_espacos(Espacos,Permutacoes),
    simplifica(Permutacoes,Perms_Possiveis).

/*Funcoes auxiliares para utilizar o nth1 na Perm e colocar em Vars*/
aux_nth(Perm, Vars):-
    nth1(1, Perm, Vars).

aux_nth2(Perm, Permutacoes):-
    nth1(2, Perm, Permutacoes).

/*Funcao auxiliar do predicado 16 que recebe 2 listas, uma com os numeros de 1 ate
o numero de espacos do Permutacoes Possiveis com mais do que 1 permutacao e outra com
o tamanho da lista de permutacoes de cada espaco com mais do que 1 permutacao e devolve
o valor minimo da segunda lista*/
verifica_minimo([Head|Tail], [Head2|Tail2], Menor, Minimo):-
    (Head=:=Menor-> Minimo is Head2,!
    ;
    verifica_minimo(Tail, Tail2, Menor, Minimo)).
verifica_minimo([],_,_,_).
verifica_minimo(_,[],_,_).

/*Predicado 16: Recebe uma lista de permutacoes possiveis e retorna uma
Escolha, que e o primeiro espaco que tem cumulativamente mais do que uma
permutacao e tem o numero minimo de permutacoes*/
escolhe_menos_alternativas(Perms_Possiveis,Res):-
    maplist(aux_nth, Perms_Possiveis, Vars),
    maplist(aux_nth2, Perms_Possiveis, Permutacoes),
    findall(Tamanho, (member(Perm, Permutacoes), length(Perm,Tamanho)), Lista_Tamanhos),
    exclude(==(1), Lista_Tamanhos, Lista_Tamanho_Sem_1),
    min_list(Lista_Tamanho_Sem_1, Menor),
    length(Lista_Tamanhos,Tamanho),
    numlist(1,Tamanho,Lista_Indices),
    verifica_minimo(Lista_Tamanhos, Lista_Indices, Menor, Minimo),!,
    nth1(Minimo,Vars,Vars_Minimo),
    nth1(Minimo,Permutacoes,Permutacoes_Minimo),
    Res=[Vars_Minimo,Permutacoes_Minimo].

/*Funcao auxiliar do predicado 17 que recebe um espaco e vai construindo novamente
a lista de permutacoes possiveis ate encontrar o espaco na lista. Quando isto 
acontece, o espaco e substituido e continuamos a preencher a nova lista a partir
dai.*/
experimenta_perm_aux(Perms_Possiveis, Espaco, Novas_Perms_Possiveis):-
experimenta_perm_aux2(Perms_Possiveis, Espaco, Novas_Perms_Possiveis, Res),
Novas_Perms_Possiveis=Res.

experimenta_perm_aux2([Head|Tail], Espaco, Novas_Perms_Possiveis, [Elemento|Res]):-
    nth1(1, Head, Vars),
    nth1(2, Head, Perms),
    (Vars==Espaco->exclude(\=(Espaco), Perms, Novas_Perms), Elemento=[Vars, Novas_Perms]
    ;
    Elemento=[Vars,Perms]),
    experimenta_perm_aux2(Tail, Espaco, Novas_Perms_Possiveis, Res),!.
experimenta_perm_aux2([],_,_,[]).

/*Predicado 17: Recebe uma escolha, que representa um espaco, uma lista com as
permutacoes possiveis e devolve uma lista com as novas perms possiveis, que resultam
de unificar o espaco da Escolha com uma das permutacoes da escolha e substituir o espaco
escolha nas Perms Possiveis por esta unificacao e retornar as Novas Permutacoes Possiveis*/
experimenta_perm(Escolha,Perms_Possiveis,Novas_Perms_Possiveis):-
    nth1(1,Escolha,Espaco),
    nth1(2,Escolha,List_Perms),
    member(Perm,List_Perms),
    Espaco=Perm,
    experimenta_perm_aux(Perms_Possiveis, Espaco, Novas_Perms_Possiveis).

/*Funcao auxiliar que serve de caso terminal ao predicado 18, ou seja, quando 
todas as permutacoes tem tamanho 1, a recursao deve terminar e e isto que esta
funcao verifica, devolvendo True caso isto aconteca.*/
verifica_perms(Perms_Possiveis):-
    forall((nth1(2, Perm, Permutacoes), member(Perm, Perms_Possiveis)), 
    (length(Permutacoes,Tamanho), Tamanho=:=1)).

/*Funcao auxiliar do predicado 18 que verifica se existe alguma lista de permutacoes 
com tamanho diferente de 0, porque quando isto acontece e sinal de que a escolha foi
mal feita e o Prolog deve retroceder*/
verifica_aux_0(Perms_Possiveis):-
    forall((nth1(2, Perm, Permutacoes), member(Perm, Perms_Possiveis)),
    (length(Permutacoes,Tamanho), Tamanho=\=0)).

/*Predicado 18: Aplica o escolhe menos alternativas, o experimenta perm e o simplifica
ate todas as listas de permutacoes dos espacos terem uma unica permutacao, ou seja, a 
solucao do Puzzle. Caso a escolha feita no escolhe menos alternativas resulte num 
conjunto de permutacoes impossiveis, o Prolog retorna a ultima iteracao e parte dessa
escolha para continuar*/
resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis):-
    (verifica_perms(Perms_Possiveis)-> Novas_Perms_Possiveis=Perms_Possiveis, !
    ;
    escolhe_menos_alternativas(Perms_Possiveis,Escolha),
    experimenta_perm(Escolha,Perms_Possiveis, Permutacoes),
    simplifica(Permutacoes,Novas_Perms),
    verifica_aux_0(Novas_Perms),
    resolve_aux(Novas_Perms,Novas_Perms_Possiveis)).

/*Funcao principal do jogo. Resolve totalmente o Puzzle, procedendo a sua inicializacao
e posterior resolucao.*/
resolve(Puz):-
    inicializa(Puz,Perms_Possiveis),
    resolve_aux(Perms_Possiveis,Novas_Perms_Possiveis),
    Novas_Perms_Possiveis=Novas_Perms_Possiveis.







