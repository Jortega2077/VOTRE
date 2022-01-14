***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCC22R (CPY VTMCCCAR)                           *                         
     ** Descripciòn:  : Consulta de Archivos a Subir (Bancos)             *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : Leonel Mauricio Parra Suárez - PersonalSoft       *                         
     ** Responsable   : (Jortega)                                         *                         
     ** Fecha Creaciòn: 06 de Agosto de 2018                              *                         
     **                                                                   *                         
      * Modo de Compilación del Programa:                                    *                      
      *                                                                      *                      
      * Modulo:                                                              *                      
      *     CRTSQLRPGI OBJ(VOTREP00/VTMCC22R) SRCFILE(VOTREP00/QRPGLESRC)    *                      
      *                SRCMBR(VTMCC22R) OBJTYPE(*MODULE)                     *                      
      *                TEXT('Modulo Genera Archivos a Subir ')               *                      
      *                CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE)                   *                      
      *                                                                      *                      
     **                                                                   **                        
     ** Definición de Directivas de Compilación                                                     
     H Bnddir('HTTPAPI':'QC2LE')                                                                    
     H Timfmt(*Hms)                                                                                 
     H Option(*NoDebugIO: *ShowCpy:*NoXRef:*NoExpDds:*NoSecLvl:*NoExt:*SrcStMt)                     
     H DatFmt(*Iso) Aut(*all)                                                                       
                                                                                                    
       Ctl-Opt Nomain;                                                                              
       Dcl-s W_Dato Char(100);                                                                      
         // Declaracion de Variables de Trabajo                                                     
         Dcl-s w_Cia Int(3);                                                                        
         Dcl-s Rp    Int(10);                                                                       
         Dcl-s RUTAELI Char(100);                                                                   
         Dcl-s Pos Int(3);                                                                          
         Dcl-s Ruta Char(70);                                                                       
         Dcl-s w_usr Char(10)  Inz(*user);                                                          
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
         Dcl-s Datos  Char(1);                                                                      
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
         Dcl-s Pcddtl Char(10);                                                                     
         Dcl-s Pcddtf Char(10);                                                                     
         Dcl-s Preg Char(1000);                                                                     
         Dcl-s Pos3 Zoned(3);                                                                       
         Dcl-s Pos4 Zoned(3);                                                                       
         Dcl-s Pos5 Zoned(9);                                                                       
         Dcl-s w_ciclo Zoned(1);                                                                    
       dcl-ds IFS_Status qualified template;                                                        
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
     DDirent           Ds                  Based(P_dirent)                                          
     Dd_reserved1                    16a                                                            
     D   D_fileno_gen_id...                                                                         
     D   D_fil                       10u 0                                                          
     D   D_fileno                    10u 0                                                          
     D   D_reclen                    10u 0                                                          
     D   D_reserved3                 10i 0                                                          
     D   D_reserved4                  8a                                                            
     D   D_nlsinfo                   12a                                                            
     D     Nls_ccsid                 10i 0 Overlay(D_nlsinfo:1)                                     
     D     Nls_cntry                  2a   Overlay(D_nlsinfo:5)                                     
     D     Nls_lang                   3a   Overlay(D_nlsinfo:7)                                     
     D     Nls_reserv                 3a   Overlay(D_nlsinfo:10)                                    
     D   Len                         10u 0                                                          
     D   D_NAME                     640a                                                            
       Dcl-Ds Fields ExtName('VTMASBF') prefix(Q) end-ds;                                           
       Dcl-ds Fields1 Extname('VTTRBSF') Prefix(Q) End-Ds;                                          
       Dcl-pr AtrIfs  extpgm('VOTREP00/VTMGOBJR');                                                  
               RutaIfs char(100);                                                                   
               FechaAc char(8);                                                                     
       End-pr;                                                                                      
       Dcl-pr qcmdexc extpgm('QCMDEXC');                                                            
               theCmd char(3000) const;                                                             
               cmdLne packed(15 : 5) const;                                                         
               dbcs char(3) const options(*nopass);                                                 
       End-pr;                                                                                      
                                                                                                    
      ** Definiciòn de Variables de Trabajo                                                         
                                                                                                    
      ** Elimina Archivo                                                                            
         dcl-ds dsStat likeds(IFS_Status);                                                          
                                                                                                    
         dcl-pr Opendir pointer extproc('opendir');                                                 
           dirname pointer value options(*string);                                                  
         end-pr;                                                                                    
         dcl-pr readdir pointer extproc('readdir');                                                 
            Pdir     pointer value options(*string);                                                
         End-pr;                                                                                    
     DClosedir         Pr            10i 0 Extproc('closedir')                                      
     DDirname                          *   Value Options(*String)                                   
     DRead             Pr            10i 0 Extproc('read')                                          
     DFildes                         10i 0 Value                                                    
     DBuf                              *   Value                                                    
     DNroBytes                       10u 0 Value                                                    
                                                                                                    
      ** Abre Archivo                                                                               
     DOpen             Pr            10i 0 ExtProc('open')                                          
     DNombreArchivo                    *   Value Options(*String)                                   
     DParametrosOpen                 10i 0 Value                                                    
     DModoOpen                       10u 0 Value Options(*NoPass)                                   
     DCodePage                       10u 0 Value Options(*NoPass)                                   
                                                                                                    
      ** Cierra Archivo                                                                             
     DClose            Pr            10i 0 ExtProc('close')                                         
     DFileOpen                       10i 0 Value                                                    
                                                                                                    
      ** Elimina Archivo                                                                            
     DBorrarFile       Pr            10I 0 ExtProc('unlink')                                        
     D                                 *   Value  Options(*String)                                  
         /////////////////////////////////////////////////////////////////////                      
         //Proptotipo Fn_RecuperaRegistoI                                   //                      
         /////////////////////////////////////////////////////////////////////                      
                                                                                                    
       Dcl-Proc Fn_RecuperaRegistoI Export;                                                         
        Dcl-Pi *n Char(1000);                                                                       
           Pcia  Char(3);                                                                           
           Prut  Char(64);                                                                          
           xfile Char(500);                                                                         
           ireg  Char(1000);                                                                        
         dsfilep likeds(Fields);                                                                    
        End-Pi;                                                                                     
         wrut=Prut;                                                                                 
         Fields = dsfilep;                                                                          
         w_file = xfile;                                                                            
         w_Pcia = Pcia;                                                                             
         w_conve =  dsfilep.QASBCNV;                                                                
         w_arch  = dsfilep.QASBTPF;                                                                 
         Pcdbay  = %Char(dsfilep.QASBCDB);                                                          
         Pcdesy  = %Char(dsfilep.QASBCDE);                                                          
         Pcddtl  = %Char(dsfilep.QAsbCdt);                                                          
         Pcddtf  = dsfilep.QAsbDtf;                                                                 
         Clear Preg;                                                                                
         GenerarPreg();                                                                             
         GrabarReg();                                                                               
         Return Preg;                                                                               
       End-proc;                                                                                    
       //-------------------------------------------------------------------//                      
       //Nombre Proc.: GenerarPreg                                          //                      
       //Propósito...:                                                      //                      
       //                                                                   //                      
       //---------------------------------------------------------------------                      
       Dcl-Proc GenerarPreg;                                                                        
        Monitor;                                                                                    
         // Abro Directorio                                                                         
         FileW = *Blanks;                                                                           
         Ruta = *Blanks;                                                                            
         Ruta = %Trim(wrut);                                                                        
         Pos1 = %Scan(' ':Ruta);                                                                    
         Dir = OpenDir(%Subst(Ruta:1:Pos1-1));                                                      
         // Leo Contenido del Directorio                                                            
          P_dirent = ReadDir(Dir);                                                                  
         // Mientras que Hallan Elementos dentro de la Carpeta                                      
          DoW P_dirent <> *Null;                                                                    
           FileW = %Subst(D_name:1:Len);                                                            
           If %SubSt(D_name  :1:1) <>'.';                                                           
              If %SubSt(D_name  :1:1) <>'.' And %Trim(FileW) = %Trim(fileW);                        
               // Procesar Archivo de Respuestas                                                    
                ProcesarArchivoPReg();                                                              
                Leave;                                                                              
              Endif;                                                                                
           Endif;                                                                                   
            // Leo Siguiente Entrada de Directorio                                                  
            P_dirent = ReadDir(Dir);                                                                
           EndDo;                                                                                   
          // Cierra Directorio                                                                      
          Rp = Closedir(Dir);                                                                       
        On-Error;                                                                                   
         // Cierra Directorio                                                                       
         Rp = Closedir(Dir);                                                                        
        EndMon;                                                                                     
       End-Proc GenerarPreg;                                                                        
       //-------------------------------------------------------------------//                      
       //Nombre Proc.: ProcesarArchivoPreg                                  //                      
       //Propósito...:                                                      //                      
       //                                                                   //                      
       //---------------------------------------------------------------------                      
       Dcl-Proc ProcesarArchivoPreg;                                                                
        Dcl-s Null Char(2) Inz(X'0020');                                                            
        Dcl-s Pos  Zoned(2);                                                                        
        Dcl-s Pos2 Zoned(3);                                                                        
        Dcl-s LineMax Zoned(10);                                                                    
        Dcl-c O_Rdonly   1;                                                                         
        Dcl-c O_Textdata 16777216;                                                                  
        Dcl-c Cr         x'0D';                                                                     
        Dcl-c Lf         x'25';                                                                     
        Clear w_ciclo;                                                                              
        CLear pos3;                                                                                 
        File = d_name;                                                                              
        Pos = %Scan(%SubSt(Null:1:1):File);                                                         
        If Pos > *Zeros;                                                                            
         File = %SubSt(File:1:Pos-1);                                                               
         // Abro File                                                                               
         FileX = %Subst(Ruta:1:Pos1-1) + '/' + %Trim(%SubSt(File:1:Pos-1));                         
         Pos2 = %Scan(' ':FileX);                                                                   
         Rp = Open(%Subst(FileX:1:Pos2-1):O_Textdata + O_Rdonly:819:0);                             
         // Si se Puede Abrir el Archivo                                                            
         If Rp >= *Zeros ;                                                                          
          // Leemos Registros del Archivo                                                           
          LineMax = %Size(DatosR) - 2;                                                              
          Dow (Read(Rp:%Addr(Datos):%Size(Datos)) = %Size(Datos));                                  
           If (Datos <> Cr And Datos <> Lf);                                                        
              Datos = %Xlate(' ':'!':Datos);                                                        
              DatosR = %Trim(DatosR) + Datos;                                                       
            //Preg = Datosr;                                                                        
           Endif;                                                                                   
           If (Datos = Lf Or %Len(DatosR) = LineMax);                                               
            pos3 = %len(%Trim(Pcddtl));                                                             
            Pos4 = %len(%Trim(Pcddtf));                                                             
            Datosr = %Xlate('!':' ':Datosr);                                                        
            Pos5 = %len(%Trim(Datosr));                                                             
            If %Len(%Trim(DatosR)) > 1                                                              
               And ((%Subst(DatosR:1:pos3) = %Subst(Pcddtl:1:pos3)                                  
                     And pos3 > *zeros)                                                             
               Or (%Subst(DatosR:(Pos5+1)-pos4:Pos4) =                                              
                   %Subst(Pcddtf:1:pos4)                                                            
                   And pos4>*Zeros));                                                               
               Preg = %trim(Preg)+%Trim(Datosr);                                                    
               Clear Datosr;                                                                        
               If w_ciclo = 1;                                                                      
                  Leave;                                                                            
               EndIf;                                                                               
               w_ciclo += 1;                                                                        
            Else;                                                                                   
               Clear Datosr;                                                                        
            Endif;                                                                                  
           EndIf;                                                                                   
          EndDo;                                                                                    
         // Cerramos Archivo                                                                        
          DatosR = *Blanks;                                                                         
         // Cerramos Archivo                                                                        
         Rp=Close(Rp);                                                                              
                                                                                                    
         Endif;                                                                                     
        Endif;                                                                                      
       End-Proc ProcesarArchivoPreg;                                                                
       //-------------------------------------------------------------------//                      
       //Nombre Proc.: GrabarReg                                            //                      
       //Propósito...:                                                      //                      
       //                                                                   //                      
       //---------------------------------------------------------------------                      
       Dcl-Proc GrabarReg;                                                                          
      ** Definición de Variables de Trabajo                                                         
         Dcl-s WaÑo    Char(4) Inz(*Blanks);                                                        
         Dcl-s Wmes    Char(2) Inz(*Blanks);                                                        
         Dcl-s Wdia    Char(2) Inz(*Blanks);                                                        
         Dcl-s Curtime TIME Inz(*Sys);                                                              
         Dcl-s Kfecha  Char(10) Inz(*Blanks);                                                       
         Dcl-s w_Count Zoned(9) Inz(*Zeros);                                                        
                                                                                                    
         Clear w_Count;                                                                             
         Clear Fields1;                                                                             
         WaÑo = %subst(%Char(%date():*iso):1:4);                                                    
         Wmes = %subst(%Char(%date():*iso):6:2);                                                    
         Wdia = %subst(%Char(%date():*iso):9:2);                                                    
         Kfecha = WaÑo + '/' + Wmes + '/' + Wdia;                                                   
         Exec Sql                                                                                   
         Select Trim(Replace(:PReg, '!', ' ')) Into :PReg                                           
         From Sysibm/Sysdummy1;                                                                     
         Exec Sql                                                                                   
         Select count(0) into :w_Count                                                              
         From VTTRBSF                                                                               
         Where RBSCIA = :w_Pcia                                                                     
           And RBSCBA =  int(:Pcdbay)                                                               
           And RBSRAR =  Trim(:PReg);                                                               
         //And RBSFCA = :Kfecha;                                                                    
         If w_count = *Zeros;                                                                       
          QRbscia = w_Pcia  ;                                                                       
          QRbsfca = WaÑo + '/' + Wmes + '/' + Wdia;                                                 
          QRbshca = %Char(Curtime:*Hms);                                                            
          QRbscba = %int(Pcdbay);                                                                   
          QRbsces = %int(Pcdesy);                                                                   
          Exec Sql Select BCPBAN, BCPNOM into  :QRbsnba , :QRbsnes                                  
          From votrea00.VTTBCPF                                                                     
          Where BCPCIA = :w_Pcia                                                                    
            and BCPCBA = :QASBCDB and BCPCES = :QASbcde;                                            
          QRbsfar = *Blanks;                                                                        
          QRbstre = *Zeros;                                                                         
          QRbsvto = *Zeros;                                                                         
          QRbsusr = w_usr;                                                                          
                                                                                                    
          QRbsnar = filew;                                                                          
          QRbsrar = %Trim(PReg);                                                                    
          QRbsinp = 'P';                                                                            
          Exec Sql                                                                                  
          Insert into VTTRBSF  Values(:Fields1);                                                    
         Endif;                                                                                     
       End-Proc GrabarReg;                                                                          
