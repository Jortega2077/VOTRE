***************************************************************************                         
      *                                                                        *                    
      * Programa      : VTMCC21R (CPY VTMCCC6R)                                *                    
      * Descripciòn:  : Modulo Genera Archivos a Subir                         *                    
      * Proyecto      : Menú de Opciones Opi Paises                            *                    
      * Autor         : Leonel Mauricio Parra Suárez - PersonalSoft            *                    
      * Responsable   : (Jortega)                                              *                    
      * Fecha Creaciòn: 06 de Agosto de 2018                                   *                    
      **************************************************************************                    
      * Modo de Compilación del Programa:                                      *                    
      *                                                                        *                    
      * Modulo:                                                                *                    
      *     CRTSQLRPGI OBJ(VOTREP00/VTMCC21R) SRCFILE(VOTREP00/QRPGLESRC)      *                    
      *                SRCMBR(VTMCC21R) OBJTYPE(*MODULE)                       *                    
      *                TEXT('Modulo Genera Archivos a Subir ')                 *                    
      *                CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE)                     *                    
      *                                                                        *                    
      **************************************************************************                    
       Ctl-Opt Bnddir('HTTPAPI':'QC2LE');                                                           
       Ctl-Opt Timfmt(*Hms);                                                                        
       Ctl-Opt Option(*NoDebugIO: *ShowCpy:*NoXRef:*NoExpDds                                        
                     :*NoSecLvl:*NoExt:*SrcStMt);                                                   
       Ctl-Opt DatFmt(*Iso) Aut(*all);                                                              
       Ctl-Opt Nomain;                                                                              
                                                                                                    
       // Declaracion de Variables de Trabajo                                                       
       Dcl-s w_Cia Int(3);                                                                          
       //Dcl-s Rp    Int(10);                                                                       
     DRp               s             20i 0                                                          
       Dcl-s RUTAELI Char(100);                                                                     
       Dcl-s Pos Int(3);                                                                            
       Dcl-s Ruta Char(70);                                                                         
       Dcl-s Prut Char(64);                                                                         
       Dcl-s wrut Char(64);                                                                         
       Dcl-s File Char(640);                                                                        
       Dcl-s RutaIfs Char(70);                                                                      
       Dcl-s FechaAc Char(8);                                                                       
       Dcl-s NombreRIfsT  Char(8);                                                                  
       Dcl-s FechaIfsT Char(10);                                                                    
       Dcl-s Dir POINTER;                                                                           
       Dcl-s P_Dirent  POINTER;                                                                     
       DCL-S DIRNAME POINTER;                                                                       
       Dcl-s Pos1  Int(3);                                                                          
       Dcl-s Tfecha Char(20);                                                                       
       Dcl-s Datos  Char(320000);                                                                   
       Dcl-s DatosR Char(1000);                                                                     
       Dcl-s filew  Char(500);                                                                      
       Dcl-s filex  Char(500);                                                                      
       Dcl-s w_conve Char(50);                                                                      
       Dcl-s w_Pcia   Char(3);                                                                      
       Dcl-s w_File  Char(20);                                                                      
       Dcl-s w_arch Char(1);                                                                        
       Dcl-s w_res  Char(1);                                                                        
       Dcl-s Pcdbay Char(2);                                                                        
       Dcl-s Pcdesy Char(3);                                                                        
       Dcl-s w_Pcdbay Char(2);                                                                      
       Dcl-s w_Pcdesy Char(3);                                                                      
       Dcl-s Pcddtl Char(10);                                                                       
       Dcl-s Pcdcen Char(10);                                                                       
       Dcl-s Preg Char(1000);                                                                       
       Dcl-s w_Proceso Char(1) Inz(*Blanks);                                                        
       Dcl-s W_Dato Char(100);                                                                      
       Dcl-s  theCmd Char(3000);                                                                    
       Dcl-s w_usr Char(10)  Inz(*user);                                                            
       Dcl-s PNror  Zoned(4);                                                                       
       Dcl-s Pnbnc  char(40);                                                                       
       Dcl-s W_WFILE  char(10);                                                                     
                                                                                                    
       // Declara Estructuras de trabajo                                                            
       Dcl-ds IFS_Status qualified template;                                                        
        mode     uns(10);                                                                           
        ino       uns(10);                                                                          
        nlink     int(5);                                                                           
        pad       char(2);                                                                          
        uid       uns(10);                                                                          
        gid       uns(10);                                                                          
        size     int(10);                                                                           
        atime     int(10);                                                                          
        mtime     int(10);                                                                          
        ctime     int(10);                                                                          
        dev       uns(10);                                                                          
        blksize   uns(10);                                                                          
        alcsize   uns(10);                                                                          
        objtype   char(12);                                                                         
        codepage uns(5);                                                                            
        reserved1 char(62);                                                                         
        inogenid uns(10);                                                                           
       End-ds;                                                                                      
                                                                                                    
       dcl-ds DirEnt likeds(IFS_DirEnt) based(p_Dirent);                                            
                                                                                                    
       dcl-ds ifs_DirEnt ;                                                                          
        d_reserved1 char(16);                                                                       
        reserved2 uns(10);                                                                          
        D_fileno  uns(10);                                                                          
        D_reclen  uns(10);                                                                          
        D_reserved3 int(10);                                                                        
        D_reserved4 char(8);                                                                        
        D_nlsinfo char(12);                                                                         
          Nls_ccsid   Int(10) Overlay(D_nlsinfo:1);                                                 
          Nls_cntry  Char( 2) Overlay(D_nlsinfo:5);                                                 
          Nls_lang   Char( 3) Overlay(D_nlsinfo:7);                                                 
          Nls_reserv Char( 3) Overlay(D_nlsinfo:10);                                                
        Len       uns(10);                                                                          
        D_NAME    char(640);                                                                        
       end-ds;                                                                                      
       Dcl-Ds Fields ExtName('VTMASBF') prefix(Q) end-ds;                                           
       Dcl-ds Fields1 Extname('VTTRBSF') Prefix(Q) End-Ds;                                          
       Dcl-pr AtrIfs  extpgm('VOTREP00/VTMGOBJR');                                                  
         RutaIfs char(100);                                                                         
         FechaAc char(8);                                                                           
       End-pr;                                                                                      
       Dcl-pr VTMCC23RP;                                                                            
        Pcia   char( 3);                                                                            
        Prut   char(64);                                                                            
        pfile  char(500);                                                                           
        Pcdbay char( 2);                                                                            
        Pcdesy char( 3);                                                                            
        Parch  char( 1);                                                                            
        Pdsasd likeds(Fields);                                                                      
        Pres   char( 1);                                                                            
       End-pr;                                                                                      
       Dcl-pr VTMCC24RP;                                                                            
        Pcia   char(3);                                                                             
        Pfile  char(10);                                                                            
        Pnbnc  char(40);                                                                            
        Pcbnx  char(2);                                                                             
        P_cest  char(3);                                                                            
        Pdarc  char(1000);                                                                          
        PNror  Zoned(   4);                                                                         
       End-pr;                                                                                      
       Dcl-pr qcmdexc extpgm('QCMDEXC');                                                            
        theCmd char(3000) const;                                                                    
        cmdLne packed(15 : 5) const;                                                                
        dbcs char(3) const options(*nopass);                                                        
       End-pr;                                                                                      
       Dcl-pr VTMCC21L extpgm('VOTREP00/VTMCC21L');                                                 
        Pcia   char(3);                                                                             
        Pserv  char(100);                                                                           
       End-pr;                                                                                      
                                                                                                    
      ** Definiciòn de Variables de Trabajo                                                         
       Dcl-ds dsStat likeds(IFS_Status);                                                            
                                                                                                    
       Dcl-pr Opendir pointer extproc('opendir');                                                   
         dirname pointer value options(*string);                                                    
       End-pr;                                                                                      
       Dcl-pr readdir pointer extproc('readdir');                                                   
          Pdir     pointer value options(*string);                                                  
       End-pr;                                                                                      
      ** Cierra Directorio                                                                          
       Dcl-pr closedir uns(10) extproc('closedir');                                                 
        Dirname pointer value options(*string);                                                     
       End-pr;                                                                                      
      ** Elimina Archivo                                                                            
       Dcl-pr BorrarFile uns(10) extproc('unlink');                                                 
        P_IfsFile pointer value options(*string);                                                   
       End-pr;                                                                                      
      ** Leer Archivo                                                                               
       Dcl-pr Read  uns(10) Extproc('read');                                                        
        Fildes    uns(10);                                                                          
        Buf       pointer value;                                                                    
        NroBytes  uns(10);                                                                          
       End-pr;                                                                                      
                                                                                                    
      ** Abre Archivo                                                                               
       Dcl-pr Open  uns(10) Extproc('open');                                                        
        NombreArchivo  pointer value Options(*String);                                              
        ParametrosOpen int(10) Value;                                                               
        ModoOpen uns(10) Value Options(*NoPass);                                                    
        CodePage uns(10) Value Options(*NoPass);                                                    
       End-pr;                                                                                      
                                                                                                    
      ** Cierra Archivo                                                                             
       Dcl-pr Close uns(10) Extproc('close');                                                       
        FileOpen int(10) Value;                                                                     
       End-pr;                                                                                      
       Dcl-Pr    Fn_RecuperaRegistoI   Char(1000);                                                  
            Pcia  Char(3);                                                                          
            Prut  Char(64);                                                                         
            xfile Char(500);                                                                        
            Preg  Char(1000);                                                                       
            dsfilep likeds(Fields);                                                                 
       End-pr;                                                                                      
          //-------------------------------------------------------------//                         
          //Nombre Func.: Fn_SubirArchivoaIFS                            //                         
          //Propósito...: Consultar la compañia                          //                         
          //Parámetros..: Compañia                                       //                         
          //Retorna.....: Compañia convertida                            //                         
          //-------------------------------------------------------------//                         
                                                                                                    
       Dcl-Proc Fn_SubirArchivoaIFS Export;                                                         
        Dcl-Pi *n Char(3);                                                                          
         Pcia Char(3);                                                                              
         Prut Char(64);                                                                             
         PServ Char(100);                                                                           
         dsfilep likeds(Fields);                                                                    
        End-Pi;                                                                                     
         Clear w_Proceso;                                                                           
         Clear wrut;                                                                                
         wrut = Prut;                                                                               
         Fields = dsfilep;                                                                          
         w_Pcia = Pcia;                                                                             
         Prut = '/QDLS/CIA' + Pcia + '/BANCOS/RECIBIR';                                             
         w_conve = dsfilep.QASBCNV;                                                                 
         w_arch  = dsfilep.QASBTPF;                                                                 
         Pcdbay  = %Char(dsfilep.QASBCDB);                                                          
         Pcdesy  = %Char(dsfilep.QASBCDE);                                                          
         Pcddtl  = %Char(dsfilep.QAsbCdt);                                                          
         Pcdcen  = %Char(dsfilep.QAsbcen);                                                          
         PrdatosArc();                                                                              
         Generar();                                                                                 
         //PrdatosArc1();                                                                           
         //Clear PServ;                                                                             
         //Exec Sql                                                                                 
         //Select FTPSVR into :PServ                                                                
         //From VTMFTPF                                                                             
         //Where FTPAPL = Trim(:QAsbapl)                                                            
         //  and FTPEVT = :QAsbevt;                                                                 
                                                                                                    
         if w_Proceso ='S';                                                                         
            VTMCC21L(Pcia:PServ);                                                                   
         EndIf;                                                                                     
         Return pcia;                                                                               
       End-proc;                                                                                    
       //-------------------------------------------------------------------//                      
       //Nombre Proc.: PrdatosArc                                           //                      
       //Propósito...:                                                      //                      
       //                                                                   //                      
       //---------------------------------------------------------------------                      
       Dcl-Proc PrdatosArc;                                                                         
                                                                                                    
        Exec Sql                                                                                    
          Delete From Vtwftpf6;                                                                     
                                                                                                    
        Exec SQl                                                                                    
        Select trim(a.FTPDES) into :W_Dato                                                          
        from input a                                                                                
        where  rrn(a)=1;                                                                            
        Exec Sql                                                                                    
          Insert Into Vtwftpf6 Values (:W_Dato);                                                    
        Clear theCmd;                                                                               
        theCmd = 'RGZPFM  FILE(VTWFTPF6) ';                                                         
        Callp(E) qcmdexc(theCmd:%Size(theCmd));                                                     
       End-Proc PrdatosArc;                                                                         
       //-------------------------------------------------------------------//                      
       //Nombre Proc.: Generar                                              //                      
       //Propósito...:                                                      //                      
       //                                                                   //                      
       //---------------------------------------------------------------------                      
       Dcl-Proc Generar;                                                                            
         // Abro Directorio                                                                         
         FileW = *Blanks;                                                                           
         Ruta = *Blanks;                                                                            
         Ruta = %Trim(Prut);                                                                        
         Pos1 = %Scan(' ':Ruta);                                                                    
         Dir = OpenDir(%Subst(Ruta:1:Pos1-1));                                                      
         // Leo Contenido del Directorio                                                            
         P_dirent = ReadDir(Dir);                                                                   
         // Mientras que Hallan Elementos dentro de la Carpeta                                      
         DoW P_dirent <> *Null;                                                                     
             Clear FileW;                                                                           
             FileW = %Subst((dirent.D_name):1:(dirent.Len));                                        
             If %SubSt((dirent.D_name):1:1) <>'.';                                                  
                // Procesar Archivo de Respuestas                                                   
                ProcesarArchivo();                                                                  
                //Elimina archivo procesado                                                         
                Clear RutaEli;                                                                      
                RutaEli = %Trim(Ruta) + '/' + %trim((dirent.D_name));                               
                Monitor;                                                                            
                 Rp = BorrarFile(RutaEli);                                                          
                On-Error;                                                                           
                EndMon;                                                                             
                dirent.D_name=*Blanks;                                                              
             Endif;                                                                                 
             // Leo Siguiente Entrada de Directorio                                                 
             P_dirent = ReadDir(Dir);                                                               
           EndDo;                                                                                   
           // Cierra Directorio                                                                     
           Rp = Closedir(Dir);                                                                      
       End-Proc Generar;                                                                            
       //-------------------------------------------------------------------//                      
       //Nombre Proc.: PrdatosArc1                                          //                      
       //Propósito...:                                                      //                      
       //                                                                   //                      
       //---------------------------------------------------------------------                      
       Dcl-Proc PrdatosArc1;                                                                        
         W_Dato = 'Close';                                                                          
         Exec Sql                                                                                   
           Insert Into Vtwftpf6 Values (:W_Dato);                                                   
         W_Dato = 'Quit';                                                                           
         Exec Sql                                                                                   
           Insert Into Vtwftpf6 Values (:W_Dato);                                                   
         W_Dato = 'Exit';                                                                           
         Exec Sql                                                                                   
           Insert Into Vtwftpf6 Values (:W_Dato);                                                   
       End-Proc PrdatosArc1;                                                                        
       //-------------------------------------------------------------------//                      
       //Nombre Proc.: ProcesarArchivo                                      //                      
       //Propósito...:                                                      //                      
       //                                                                   //                      
       //---------------------------------------------------------------------                      
       Dcl-Proc ProcesarArchivo;                                                                    
        Dcl-s Null       Char(2) Inz(X'0020');                                                      
        Dcl-s Pos        Zoned(2);                                                                  
        Dcl-s Pos2       Zoned(3);                                                                  
        Dcl-s Pos3       Zoned(3);                                                                  
        Dcl-s LineMax    Zoned(10);                                                                 
        Dcl-c O_Rdonly   1;                                                                         
        Dcl-c O_Textdata 16777216;                                                                  
        Dcl-c Cr         x'0D';                                                                     
        Dcl-c Lf         x'25';                                                                     
       Clear File;                                                                                  
       Clear Pos;                                                                                   
       Clear Pos2;                                                                                  
       Clear Pos3;                                                                                  
       Clear LineMax;                                                                               
       File = (dirent.d_name);                                                                      
       Pos = %Scan(%SubSt(Null:1:1):File);                                                          
       If Pos > *Zeros;                                                                             
          File = %SubSt(File:1:Pos-1);                                                              
          // Abro File                                                                              
          Clear FileX;                                                                              
          FileX = %Subst(Ruta:1:Pos1-1) + '/' + %Trim(%SubSt(File:1:Pos-1));                        
          Pos2 = %Scan(' ':FileX);                                                                  
          Pos3 = %Scan(' ':w_conve);                                                                
          If pos3 > *Zeros;                                                                         
             pos3 -= 1;                                                                             
          EndIf;                                                                                    
          Monitor;                                                                                  
          Rp = Open(%Subst(FileX:1:Pos2-1):O_Textdata + O_Rdonly:819:0);                            
          On-Error;                                                                                 
          Endmon;                                                                                   
          // Si se Puede Abrir el Archivo                                                           
          If Rp >= *Zeros  and                                                                      
             %subst(%Trim(File):1:Pos3) = %subst(%Trim(w_conve):1:Pos3);                            
             Clear Preg;                                                                            
             Preg = Fn_RecuperaRegistoI(w_Pcia:Prut:file:Preg:Fields);                              
             Clear filew;                                                                           
             filew = File;                                                                          
             If ValidaSubidaArchivo(w_Pcia:preg:Pcdbay:Pcdesy) = *Off;                              
               VTMCC23RP(w_Pcia:Prut:file:Pcdbay:Pcdesy:w_arch:Fields:w_res);                       
                If QASBINP = 'S';                                                                   
                   //Ejecuta Modulo de Posto en cartera                                             
                   Clear PNror;                                                                     
                   Clear Pnbnc;                                                                     
                   Clear w_WFile;                                                                   
                   Exec Sql                                                                         
                   Select  Rbsnba, Rbsarc INTO :Pnbnc, :w_WFile                                     
                   From Vttrbsf                                                                     
                   Where Rbscia =:w_Pcia                                                            
                     And Rbscba =Int(:Pcdbay)                                                       
                     And Rbsces =Int(:Pcdesy)                                                       
                     And Rbsarc <> ' '                                                              
                     And Rbsrar =:preg;                                                             
                   Clear w_Pcdbay;                                                                  
                   Clear w_Pcdesy;                                                                  
                   w_Pcdbay  = %Editc(QASBCDB:'X');                                                 
                   w_Pcdesy  = %Editc(QASBCDE:'X');                                                 
                   VTMCC24RP(w_Pcia:W_WFILE:Pnbnc:w_Pcdbay:w_Pcdesy:Preg:PNror);                    
                EndIf;                                                                              
             EndIf;                                                                                 
             Clear W_Dato;                                                                          
             W_Dato = 'rename '+%trim(wrut)+'/'+%Trim(FileW) + ' ' +                                
                      %trim(wrut) + 'Tmp/' + %Trim(FileW);                                          
             Exec Sql                                                                               
              Insert Into Vtwftpf6 Values (:W_Dato);                                                
             Clear W_Dato;                                                                          
             W_Dato = 'delete '+%trim(wrut)+'/'+ %Trim(FileW);                                      
             Exec Sql                                                                               
              Insert Into Vtwftpf6 Values (:W_Dato);                                                
             w_Proceso ='S';                                                                        
             DatosR = *Blanks;                                                                      
          Endif;                                                                                    
          // Cerramos Archivo                                                                       
          Rp=Close(Rp);                                                                             
       Endif;                                                                                       
       End-Proc ProcesarArchivo;                                                                    
          //-------------------------------------------------------------//                         
          //Nombre Func.: ValidaSubidaArchivo                            //                         
          //Propósito...: Consultar la compañia                          //                         
          //Parámetros..: Compañia                                       //                         
          //Retorna.....: Compañia convertida                            //                         
          //-------------------------------------------------------------//                         
                                                                                                    
       Dcl-Proc ValidaSubidaArchivo;                                                                
        Dcl-Pi *n Char(1);                                                                          
         Pcia   Char(3);                                                                            
         preg   Char(1000);                                                                         
         pcbnc  Char(2);                                                                            
         pcest  Char(3);                                                                            
        End-Pi;                                                                                     
        Dcl-s Respuesta Char(1);                                                                    
        Clear Respuesta;                                                                            
        Exec Sql                                                                                    
        Select Case when Count(0) = 0 then '0' else '1' end                                         
        Into :Respuesta                                                                             
        From Vttrbsf                                                                                
        Where Rbscia =:Pcia                                                                         
          And Rbscba =Int(:pcbnc)                                                                   
          And Rbsces =Int(:pcest)                                                                   
          And Rbsarc <> ' '                                                                         
          And Rbsrar =:preg;                                                                        
        Return Respuesta;                                                                           
       End-proc;                                                                                    
