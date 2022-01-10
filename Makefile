##
## EPITECH PROJECT, 2021
## koak
## File description:
## Makefile to build the project
##

NAME = koak

all: $(NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

re: fclean all

tests_run:
	stack test

$(NAME):
	stack install $(NAME) --local-bin-path .

.PHONY: all clean fclean re
