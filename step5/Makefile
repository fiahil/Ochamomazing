##
## Makefile
##
## Fiahil
## Fri Mar 16 2012
##

NAME	=	step5

ML	=	Case.ml		\
		Maze.ml		\
		Event.ml	\
		DrawSdl.ml	\
		Pathfinder.ml	\
		main.ml

MLI	=	Event.mli

CMO	=	$(ML:.ml=.cmo)

CMI	=	$(MLI:.mli=.cmi)

CMX	=	$(ML:.ml=.cmx)

FLAGS	=	-w Aelz

CC	=	ocamlopt

%.cmx: %.ml
	$(CC) -I +sdl bigarray.cmxa sdl.cmxa sdlloader.cmxa -o $@	-c $< $(FLAGS)

%.cmo: %.ml
	$(CC) -I +sdl bigarray.cma sdl.cma sdlloader.cma -o $@		-c $< $(FLAGS)

%.cmi: %.mli
	$(CC) -I +sdl -o $@	-c $< $(FLAGS)

all:	.dep $(CMI) $(NAME)

$(NAME):	$(CMX)
	$(CC) -I +sdl bigarray.cmxa sdl.cmxa sdlloader.cmxa -o $(NAME) $(CMX)

byte:	CC = ocamlc
byte:	clean $(CMI) $(CMO)
	$(CC) -I +sdl bigarray.cma sdl.cma sdlloader.cma -o $(NAME) $(CMO)

clean:
	@rm -f $(CMO) $(CMX) $(CMI) *.o *.cmi *.cmo *.cmx && echo "clean done"

fclean:	clean
	@rm -f $(NAME) && echo "fclean done"

re:	fclean all

.dep:
	@rm -f .depend
	@ocamldep $(MLI) $(ML) > .depend

-include .depend
