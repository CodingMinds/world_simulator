//basic socket connection demo
//31.10.2012 André Freudenreich <andre.freudenreich@gmx.net>
function[]=main()
    //config
    logfile=mopen('path\log.txt','w');
    url = 'localhost';
    port = 4567;
    
    try   
        SOCKET_open(1,url,port)
        disp("opening socket");
        mputl("opening socket", logfile);
        sleep(1000)
        output=SOCKET_read(1);
        disp(output)
        mputl("< "+output, logfile);
    catch
        disp(lasterror())
        mputl(lasterror(), logfile);
        abort
    end
    
    run=1;
    answer="";
    while run==1 then
        try     
            if answer~="" then
                output=SOCKET_query(1,answer)
                mprintf("%s\n",output)
                mputl("< "+output, logfile);   
            end
            answer =x_dialog(['enter command';'or quit'],'quit');
            mputl("> "+answer, logfile);
            if answer=="quit" then 
                run=0;
            end
        catch
            disp(lasterror())
            mputl(lasterror(), logfile);
            run=0;
        end
    end    
    disp("closing socket");
    mputl("closing socket", logfile);
    SOCKET_close(1);
    mclose(logfile);   
endfunction
