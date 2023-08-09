#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>

// A function to trim a string
char *trim(char *string)
{
    char *tmp_p;
    char delimit[] = "\t\n ";
    char *stringdup = strdup(string);
    char *newstring = strtok_r(stringdup, delimit, &tmp_p);
    return newstring;
}

// A function to print and global error message
char error_message[30] = "An error has occurred\n";
void myPrint(char *msg)
{
    write(STDOUT_FILENO, msg, strlen(msg));
}

//Function to return list of argumenets
char **list_args(char *command)
{
    char *tmp_p2;
    char **listargs = (char **)calloc(30, sizeof(char *));
    char *arguement = strtok_r(command, " ", &tmp_p2);
    int i = 0;

    while (arguement)
    {
        listargs[i] = arguement;
        i++;
        arguement = strtok_r(NULL, " ", &tmp_p2);
        //printf("%s", arguement);
    }
    listargs[i] = NULL;

    return listargs;
}

//Function to create a fork that waits for the previous child process to finish
void createfork(char *command, char **argstring)
{

    int status, pid;

    if ((pid = fork()) < 0)
    {
        myPrint(error_message);
        exit(0);
    }
    else if (pid == 0)
    {
        if (execvp(command, argstring) < 0)
        {
            myPrint(error_message);
            exit(0);
        }
    }
    else
    {
        waitpid(pid, &status, WUNTRACED);
    }
}

/* ---------------------------------------------------------------------------  MAIN ------------------------------------------------------------------------------------*/

int main(int argc, char *argv[])
{
    // Variables for detecting input and errors
    char cmd_buff[2000];
    char *pinput;

    for (int i = 0; i < 10; i++)
    {
        i++;
        if ((argv[i] == NULL) && (i > 2))
        {
            myPrint(error_message);
            exit(0);
        }
    }

    FILE *batchfile = fopen(argv[1], "r");
    if ((argv[1] != NULL) && (batchfile == NULL))
    {
        myPrint(error_message);
        exit(0);
    }
    else
    {
    }
    int counter;
    int original = dup(1);
    int outfile;
    //temporary;
    //FILE *tmpfile;
    //FILE *tmpfile2;
    int adv_redir;
    int redir;
    //char currchar;
    char *redir_filename;

    while (1)
    {

        if (argv[1] == NULL)
        {
            myPrint("myshell> ");
        }

        // --------------------------------------------------------------------------- General input
        if (argv[1] != NULL)
        {
            pinput = fgets(cmd_buff, 2000, batchfile);
        }
        else
        {
            pinput = fgets(cmd_buff, 2000, stdin);
        }
        if (!pinput)
        {
            exit(0);
        }
        if (cmd_buff[0] == '\n')
        {
            continue;
        }
        int blank = 1;
        for (int i = 0; i < strlen(cmd_buff); i++)
        {
            if ((cmd_buff[i] != ' ') && (cmd_buff[i] != '\t') && (cmd_buff[i] != '\n'))
            {
                blank = 0;
            }
        }

        if (blank == 0)
        {
            myPrint(cmd_buff);
        }
        else
        {
            continue;
        }

        // ------------------------------------------------------------------------------ Checking the length of the command line

        int toolong = 0;
        for (int i = 0; i < 2000; i++)
        {
            if (((cmd_buff[i] == '\n') || (cmd_buff[i] == '\0')) && (i < 513))
            {
                break;
            }
            else if (i > 514)
            {
                myPrint(error_message);
                toolong = 1;
                break;
            }
        }
        if (toolong == 1)
        {
            continue;
        }
        if (strcmp(cmd_buff, "exit\n") == 0)
        {
            exit(0);
        }

        // ------------------------------------------------------------------------------ Parsing commands
        char delimit[] = "\n\t;";
        char *tmp_p;
        char *tmp_p2;
        char *currentcommand;

        currentcommand = strtok_r(cmd_buff, delimit, &tmp_p);

        while (currentcommand)
        {
            counter = 0;
            adv_redir = 0;
            redir = 0;
            redir_filename = 0;

            if (strcmp(currentcommand, ">") == 0)
            {
                myPrint(error_message);
                currentcommand = strtok_r(NULL, delimit, &tmp_p);
                continue;
            }

            // ---------------------------------------------------------------------------------- Checking if advanced redirection
            if (strstr(currentcommand, ">+") != NULL)
            {
                for (int i = 0; i < strlen(currentcommand); i++)
                {
                    if (currentcommand[i] == '>')
                    {
                        counter++;
                    }
                }
                if (counter > 1)
                {
                    myPrint(error_message);
                    currentcommand = strtok_r(NULL, delimit, &tmp_p);
                    continue;
                }
                counter = 0;
                adv_redir = 1;

                for (int i = 0; i < strlen(currentcommand); i++)
                {
                    if (currentcommand[i] == '+')
                    {
                        currentcommand[i] = ' ';
                    }
                }

                currentcommand = strtok_r(currentcommand, ">", &tmp_p2);
                char *checker2 = trim(currentcommand);
                if (checker2 == NULL)
                {
                    myPrint(error_message);
                    currentcommand = strtok_r(NULL, delimit, &tmp_p);
                    continue;
                }

                if ((strcmp(checker2, "cd") == 0) || (strcmp(checker2, "pwd") == 0) || (strcmp(checker2, "exit") == 0))
                {
                    myPrint(error_message);
                    currentcommand = strtok_r(NULL, delimit, &tmp_p);
                    continue;
                }

                redir_filename = trim(strtok_r(NULL, ">", &tmp_p2));

                int checker = 0;
                checker = open(redir_filename, O_CREAT | O_EXCL | O_WRONLY, 0664);
                int checker3 = open(redir_filename, O_CREAT | O_RDWR, 0664);

                if ((checker < 1) && (checker3 > 0))
                {
                    char origibuff[2000] = {0};

                    int tmpfile = open("tmp", O_CREAT | O_RDWR | O_TRUNC, 0664);
                    int i;

                    read(checker3, origibuff, 2000);
                    for (i = 0; i < 2000; i++)
                    {
                        if (origibuff[i] == '\0')
                        {
                            break;
                        }
                    }
                    write(tmpfile, origibuff, i);

                    outfile = open(redir_filename, O_CREAT | O_RDWR | O_TRUNC , 0664);
                }
                else if ((checker < 1) && (checker3 < 1))
                {
                    myPrint(error_message);
                    currentcommand = strtok_r(NULL, delimit, &tmp_p);
                    continue;
                }
                else
                {
                    outfile = open(redir_filename, O_CREAT | O_WRONLY , 0664);
                    redir = 1;
                    adv_redir = 0;
                }
            }
            // ------------------------------------------------------------------------------------------- Checking if redirection
            else if (strstr(currentcommand, ">") != NULL)
            {

                for (int i = 0; i < strlen(currentcommand); i++)
                {
                    if (currentcommand[i] == '>')
                    {
                        counter++;
                    }
                }
                if (counter > 1)
                {
                    myPrint(error_message);
                    currentcommand = strtok_r(NULL, delimit, &tmp_p);
                    continue;
                }
                counter = 0;
                redir = 1;

                currentcommand = strtok_r(currentcommand, ">", &tmp_p2);
                char *checker = trim(currentcommand);
                if (checker == NULL)
                {
                    myPrint(error_message);
                    currentcommand = strtok_r(NULL, delimit, &tmp_p);
                    continue;
                }

                if ((strcmp(checker, "cd") == 0) || (strcmp(checker, "pwd") == 0) || (strcmp(checker, "exit") == 0))
                {
                    myPrint(error_message);
                    break;
                }

                redir_filename = (strtok_r(NULL, ">", &tmp_p2));
                if (redir_filename == NULL)
                {
                    myPrint(error_message);
                    currentcommand = strtok_r(NULL, delimit, &tmp_p);
                    continue;
                }
                redir_filename = trim(redir_filename);

                outfile = open(redir_filename, O_CREAT | O_EXCL | O_WRONLY | O_TRUNC, 0664);
                if (outfile < 1)
                {
                    myPrint(error_message);
                    currentcommand = strtok_r(NULL, delimit, &tmp_p);
                    continue;
                }
            }
            // -------------------------------------------------- Carrying out the commands with the arguements
            char **args;
            args = list_args(currentcommand);

            if (args[0] == NULL)
            {
                currentcommand = strtok_r(NULL, delimit, &tmp_p);
                continue;
            }

            char *firstarg = trim(args[0]);
            //-------------------------------------------------------------------- Check for bad formatting
            if (((args[1] != NULL) && (strcmp(firstarg, "exit") == 0)) || ((args[2] != NULL) && (strcmp(firstarg, "cd") == 0)) ||
                ((args[1] != NULL) && (strcmp(firstarg, "pwd") == 0)))
            {
                myPrint(error_message);
                currentcommand = strtok_r(NULL, delimit, &tmp_p);
                continue;
            }
            //-------------------------------------------------- exitcd
            if (strcmp(firstarg, "exit") == 0)
            {
                exit(0);
            }
            //-------------------------------------------------- change directory
            else if (strcmp(firstarg, "cd") == 0)
            {
                if (args[1] == NULL)
                {
                    char *home = getenv("HOME");
                    int changed = chdir(home);
                    if (changed < 0)
                    {
                        myPrint(error_message);
                        currentcommand = strtok_r(NULL, delimit, &tmp_p);
                        continue;
                    }
                }
                else
                {
                    int changed = chdir(args[1]);
                    if (changed < 0)
                    {
                        myPrint(error_message);
                        currentcommand = strtok_r(NULL, delimit, &tmp_p);
                        continue;
                    }
                }
            }
            // ----------------------------------------------- print working directory
            else if (strcmp(firstarg, "pwd") == 0)
            {
                char dirbuff[1000];
                getcwd(dirbuff, sizeof(dirbuff));
                myPrint(dirbuff);
                myPrint("\n");
            }
            // ---------------------------------------------------- Fork if nothing else

            else
            {
                //myPrint(args[0]);
                //myPrint(args[1]);
                //myPrint(args[2]);
                // myPrint(strtok_r(NULL, delimit, &tmp_p));
                // -------------------------------------------------- Redirection
                if ((redir == 1) || (adv_redir == 1))
                {
                    dup2(outfile, STDOUT_FILENO);
                }

                createfork(args[0], args);
                if ((redir == 1) || (adv_redir == 1))
                {
                    dup2(original, STDOUT_FILENO);
                }
            }
 
            int temporary = open("tmp", O_RDWR, 0664);
            int newfile = open(redir_filename, O_RDWR | O_APPEND, 0664);
            char buffer[2000] = {0};
            if (adv_redir == 1)
            {
                int i;
                read(temporary, buffer, 2000);
                for (i = 0; i < 2000; i++)
                {
                    if (buffer[i] == '\0')
                    {
                        break;
                    }
                }
                write(newfile, buffer, i);
            }

            currentcommand = strtok_r(NULL, delimit, &tmp_p);
        }
    }
}
