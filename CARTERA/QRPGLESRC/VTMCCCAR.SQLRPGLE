***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCCAR                                          *                         
     ** Descripciòn:  : Consulta el Archivo a Subir del Banco             *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 27 de Enero de 2009                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DDir              s               *                                                            
     DP_Dirent         s               *                                                            
     DFile             s            500a   Inz(*Blanks)                                             
     DFileX            s            500a   Inz(*Blanks)                                             
     DRp               s             10i 0                                                          
     DPos1             s              3s 0 Inz(*Zeros)                                              
     DDatos            s              1a                                                            
     DDatosR           s           1000a   Varying                                                  
     DRuta             s             70a   Inz(*Blanks)                                             
                                                                                                    
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
     D   FileName                   640a                                                            
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DGenerar          Pr                                                                           
     DProcesarArchivo  Pr                                                                           
                                                                                                    
      ** Abre Directorio                                                                            
     DOpendir          Pr              *   Extproc('opendir')                                       
     DDirname                          *   Value Options(*String)                                   
                                                                                                    
      ** Lee Directorio                                                                             
     DReaddir          Pr              *   Extproc('readdir')                                       
     DDirname                          *   Value Options(*String)                                   
                                                                                                    
      ** Cierra Directorio                                                                          
     DClosedir         Pr            10i 0 Extproc('closedir')                                      
     DDirname                          *   Value Options(*String)                                   
                                                                                                    
      ** Lee Archivo                                                                                
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
                                                                                                    
      ** Parametros de Entrada al Programa                                                          
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     DPrut                           64a                                                            
     DPfile                          20a                                                            
     DPreg                         1000a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DPrut                           64a                                                            
     DPfile                          20a                                                            
     DPreg                         1000a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
         Generar();                                                                                 
        *Inlr = *On;                                                                                
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Genera la Información para el Sub File         //                       
       //------------------------------------------------------------------//                       
     PGenerar          B                                                                            
     DGenerar          Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DFileW            s            500a   Inz(*Blanks)                                             
      /Free                                                                                         
       Monitor;                                                                                     
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
           FileW = %Subst(FileName:1:Len);                                                          
           If %SubSt(FileName:1:1) <>'.' And %Trim(FileW) = %Trim(Pfile);                           
            // Procesar Archivo de Respuestas                                                       
             ProcesarArchivo();                                                                     
             Leave;                                                                                 
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
      /End-Free                                                                                     
     PGenerar          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Lee Archivo de la Ifs                          //                       
       //------------------------------------------------------------------//                       
     PProcesarArchivo  B                                                                            
     DProcesarArchivo  Pi                                                                           
     DNull             s              2a   Inz(X'0020')                                             
     DPos              s              2s 0                                                          
     DPos2             s              3s 0                                                          
     DLineMax          s             10i 0                                                          
                                                                                                    
     DO_Rdonly         c                   1                                                        
     DO_Textdata       c                   16777216                                                 
     DCr               c                   x'0D'                                                    
     DLf               c                   x'25'                                                    
                                                                                                    
      /Free                                                                                         
       File = FileName;                                                                             
       Pos = %Scan(%SubSt(Null:1:1):File);                                                          
       If Pos > *Zeros;                                                                             
        File = %SubSt(File:1:Pos-1);                                                                
        // Abro File                                                                                
        FileX = %Subst(Ruta:1:Pos1-1) + '/' + %Trim(%SubSt(File:1:Pos-1));                          
        Pos2 = %Scan(' ':FileX);                                                                    
        Rp = Open(%Subst(FileX:1:Pos2-1):O_Textdata + O_Rdonly:819:0);                              
        // Si se Puede Abrir el Archivo                                                             
        If Rp >= *Zeros;                                                                            
         // Leemos Registros del Archivo                                                            
         LineMax = %Size(DatosR) - 2;                                                               
         Dow (Read(Rp:%Addr(Datos):%Size(Datos)) = %Size(Datos));                                   
          If (Datos <> Cr And Datos <> Lf);                                                         
           Datos = %Xlate(' ':'!':Datos);                                                           
           DatosR = %Trim(DatosR) + Datos;                                                          
           Preg = Datosr;                                                                           
          Endif;                                                                                    
          If (Datos = Lf Or %Len(DatosR) = LineMax);                                                
           If %Len(%Trim(DatosR)) > 1;                                                              
            Datosr = %Xlate('!':' ':Datosr);                                                        
            Preg = Datosr;                                                                          
            Leave;                                                                                  
           Endif;                                                                                   
           DatosR = *Blanks;                                                                        
          Endif;                                                                                    
         EndDo;                                                                                     
          DatosR = *Blanks;                                                                         
        // Cerramos Archivo                                                                         
        Rp=Close(Rp);                                                                               
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PProcesarArchivo  E                                                                            
