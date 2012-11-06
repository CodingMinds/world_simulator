///////////////////////////////////////////////////////////////////////////////
//Client (user-input)
//6.11.2012 André Freudenreich <andre.freudenreich@gmx.net>
//
//A short Scilab program which connects to the server and let user 
//interact with the world.
///////////////////////////////////////////////////////////////////////////////


function[]=main()
    clc
    //config
    //open log file
    logfile=mopen('d:\log.txt','w');
    url = "localhost";
    port = 4567;
    //intial connection
    strtoint_y = 0;
    strtoint_x = 0;
    agentpos_y_x=0;
    //intial connection in try and catch to ensure error handling
    try 
        //open a tcp socket connection id: 1 to the given url and port  
        SOCKET_open(1,url,port)
        // displaying and loging "opening socket"
        disp("opening socket");
        mputl("opening socket", logfile);
        sleep(200)
        //to ensure output for
        output=SOCKET_read(1);
        disp(output)
        //extracting matrix size from output string
        strtoint_y=string2int(output,21);
        strtoint_x=string2int(output,23);
        agentx = 1*strtoint_y;
        agenty = 1*strtoint_x;
        //agent position in y and x
        agentpos_y_x = list(agentx,agenty);
        //new matrix with y and x depending on the given matrix size
        [GRID]=gridgen(strtoint_y*2,strtoint_x*2);
        //loging
        mputl("< "+output, logfile);
    catch
        disp(lasterror())
        mputl(lasterror(), logfile);
        mclose(logfile);
        abort
    end
    
    run=1;
    answer="";
    answer_int=0;
    text_output="";
    //prebuilding agent map
    //send "evirion" to server
    //server returns the objects/environ surrounding the agent
    output_environ=SOCKET_query(1,"environ")
    mputl("< "+output_environ, logfile);
    //place objects and agent postion into the matrix 
    [GRID1,GRID]=manipulator(GRID,output_environ,agentpos_y_x);
    //
    //mainloop
    while run==1 then
        mprintf("%s\n","exploring the world..")
        disp(GRID1)
        //try and catch to ensure error handling 
        try  
            //if answer is not empty..   
            if answer~="" then
                //sending usercommands (stored in answer) to the server
                output_status=SOCKET_query(1,answer)   
                mputl("< "+output_status, logfile);
                //evaluate status replay
                [agentpos_y_x,text_output] =status_verifier(agentpos_y_x,output_status,answer);
                //send "evirion" to server
                //server returns the objects/environ surrounding the agent
                output_environ=SOCKET_query(1,"environ");
                mputl("< "+output_environ, logfile);
                //place objects and postion into the matrix 
                [GRID1,GRID]=manipulator(GRID,output_environ,agentpos_y_x);
                    clc
                    mprintf("%s\n","exploring the world..")
                    disp(GRID1)
                    mprintf("%s\n","user action: "+answer)
                    mprintf("%s\n","status reply: "+text_output)
            end
            disp("commando: 1-8 directions,9 for quit")
            //storing agent keyboard in answer
            answer=inputUser();
            mputl("> "+answer, logfile);
            if answer=="quit" then 
                run=0;
            end
        catch
            disp(lasterror())
            mputl(lasterror(), logfile);
            run=0;
        end
        clc
    end
    //end main loop
     

    disp("closing socket");
    mputl("closing socket", logfile);   
    try
        //close connection
        SOCKET_close(1);
    catch
    disp("sever made a boo boo")
    disp(lasterror())
    mputl(lasterror(), logfile);
    end
    mclose(logfile);
endfunction

// function inputUser()
// returns user keyboard input

function[answer]=inputUser()
    answer_int = mscanf("%d")
            disp(answer_int)
            if answer_int >=1  then      
                if answer_int <9 then
                    answer_string = string(answer_int);
                    answer ="";
                    answer = answer +"move ";
                    answer = answer+answer_string;      
                end       
            end
            if  answer_int==9 then
                answer ="quit";  
            end
endfunction

// fuction manipulator(matrix,server_output,postion of the agent)
// place objects and agent postion into the matrix
// objects will be placed around the agent according to his coordinates
// returns a matrix(grid) with objects
// returns a matrix(grid1) objects with agent

function[GRID1,GRID]=manipulator(GRID,output_environ,agentpos_y_x)
    agentpos_y = agentpos_y_x(1);
    agentpos_x = agentpos_y_x(2);
    GRID(agentpos_y-1,agentpos_x)=part(output_environ,13);
    GRID(agentpos_y-1,agentpos_x+1)=part(output_environ,14);
    GRID(agentpos_y,agentpos_x+1)=part(output_environ,15);
    GRID(agentpos_y+1,agentpos_x+1)=part(output_environ,16);
    GRID(agentpos_y+1,agentpos_x)=part(output_environ,17);
    GRID(agentpos_y+1,agentpos_x-1)=part(output_environ,18);
    GRID(agentpos_y,agentpos_x-1)=part(output_environ,19);
    GRID(agentpos_y-1,agentpos_x-1)=part(output_environ,20);
    GRID1 = GRID
    GRID1(agentpos_y,agentpos_x)='*';
endfunction

//function string2int(string,element index)
//extract a character from a string and converts the character to an integer value
//returns an integer

function[strtoint]=string2int(str,index)
    charakter = ascii(part(str,index));   
    select charakter
        case 48 then strtoint = 0;   
        case 49 then strtoint = 1;
        case 50 then strtoint = 2;
        case 51 then strtoint = 3;
        case 52 then strtoint = 4;
        case 53 then strtoint = 5;
        case 54 then strtoint = 6;
        case 55 then strtoint = 7;
        case 56 then strtoint = 8;
        case 57 then strtoint = 9;
    end
endfunction

//function  status_verifier(postion of the agent,server_status,user input)
//decodes status msg and change position of agent acording to the given move command
//returns new postion of agent
//returns status output for the client

function[agentpos_y_x,text_output]=status_verifier(agentpos_y_x,output_status,answer)

    string_status_code="";
    string_status_code =strcat([string_status_code,part(output_status,1)]);
    string_status_code =strcat([string_status_code,part(output_status,2)]);
    string_status_code =strcat([string_status_code,part(output_status,3)]);
    if string_status_code == '201' |  string_status_code == '202'then
       int_move_code = part(answer,6);
        select int_move_code
            case '1' then
               agentpos_y_x(1) =  agentpos_y_x(1) - 1;  
            case '2' then
               agentpos_y_x(1) = agentpos_y_x(1) - 1;  
               agentpos_y_x(2) = agentpos_y_x(2) + 1;  
            case '3' then
               agentpos_y_x(2) = agentpos_y_x(2) + 1;  
            case '4' then
               agentpos_y_x(1) = agentpos_y_x(1) + 1;  
               agentpos_y_x(2) = agentpos_y_x(2) + 1;  
            case '5' then
                agentpos_y_x(1) = agentpos_y_x(1) + 1;  
            case '6' then
               agentpos_y_x(1) = agentpos_y_x(1) + 1;  
               agentpos_y_x(2) = agentpos_y_x(2) - 1; 
            case '7' then
               agentpos_y_x(2) = agentpos_y_x(2) - 1; 
            case '8' then
               agentpos_y_x(1) = agentpos_y_x(1) - 1;  
               agentpos_y_x(2) = agentpos_y_x(2) - 1; 
        end
        text_output = "agent successfull moved";    
    end
    if string_status_code == '301' then
        text_output = "agent died";
    end
    if string_status_code == '202' then
        text_output = "agent found food";
    end
    if string_status_code == '501' then
        text_output = "world destroyed";
    end
    if string_status_code == '203' then
        text_output = "path blocked";
    end
    if string_status_code == '204' then
        text_output = "path blocked by another agent";
    end
endfunction

//function gridgen(int,int)
//build up a matrix with the given y and x values
//returns a matrix

function[GRID]=gridgen(Y,X)
  for y=1:1:Y
      for x=1:1:X
          GRID(y,x)='.';
      end
  end
endfunction
