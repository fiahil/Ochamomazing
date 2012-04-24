##
## Makefile
##
## Fiahil
## Fri Mar 16 2012
##

NAME	=	step1

ML	=	Case.ml		\
		Maze.ml		\
		Draw.ml		\
		main.ml

MLI	=	Case.mli	\
		Maze.mli	\
		Draw.mli

CMO	=	$(ML:.ml=.cmo)

CMI	=	$(MLI:.mli=.cmi)

CMX	=	$(ML:.ml=.cmx)

FLAGS	=	-w Aelz

CC	=	ocamlopt

%.cmx: %.ml
	$(CC) -o $@	-c $< $(FLAGS)

%.cmo: %.ml
	$(CC) -o $@	-c $< $(FLAGS)

%.cmi: %.mli
	$(CC) -o $@	-c $< $(FLAGS)

all:	.dep $(CMI) $(NAME)

$(NAME):	$(CMX)
	$(CC) -o $(NAME) $(CMX)

byte:	CC = ocamlc
byte:	clean $(CMI) $(CMO)
	$(CC) -o $(NAME) $(CMO)

clean:
	@rm -f $(CMO) $(CMX) $(CMI) *.o *.cmi *.cmo *.cmx && echo "clean done"

fclean:	clean
	@rm -f $(NAME) && echo "fclean done"

re:	fclean all

.dep:
	@rm -f .depend
	@ocamldep $(MLI) $(ML) > .depend

-include .depend
