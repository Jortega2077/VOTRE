      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCCUR                                          **                        
      * Descripción   : Borra Archivos de Bancos ya Trabajados            **                        
      * Proyecto      : Menú de Opciones Opi Paises                       **                        
      * Autor         : (Jortega)                                         **                        
      * Fecha Creación: 23 de Febrero de 2010                             **                        
      *                                                                   **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
     H Bnddir('QC2LE')                                                                              
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DCmd              s           3000a                                                            
     DLen              s             15p 5                                                          
     DWfecha           s             10d                                                            
     DWfechaA          s             10a   Inz(*Blanks)                                             
     DIdx              s              2s 0 Inz(1)                                                   
     DWRbsarc          s             10a   Inz(*Blanks)                                             
     DPcia             s              3a   Inz(*Blanks)                                             
     DXcia             s              3a   Inz(*Blanks)                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DPrBorrarRegPgsord...                                                                          
     D                 Pr                                                                           
     DPfileXX                        10a                                                            
     DPrPrincipal      Pr                                                                           
     DPfileYY                        10a                                                            
     DPrChequeaBloqueo...                                                                           
     D                 Pr             1n                                                            
     DPfileYY                        10a                                                            
                                                                                                    
     DPrVttrbsf        Pr                                                                           
                                                                                                    
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
                                                                                                    
     DPgmVerifica      Pr                  Extpgm('VOTREP00/VTMCCCVL')                              
     DFsPool                         10a                                                            
     DFfile                          10a                                                            
                                                                                                    
     DPrChkObjFile     Pr              n                                                            
     DOfile                          10a                                                            
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       Exec Sql                                                                                     
        Declare CY Cursor For                                                                       
        Select Char(CccmpÑ)                                                                         
        From Opcapf/Lcacmp01                                                                        
        Where Ccdele = ' ' And Substr(Ccname, 30, 1) <> 'P';                                        
                                                                                                    
       Exec Sql                                                                                     
        Open CY;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch CY Into :Xcia;                                                                        
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        Evalr Pcia = %Trim(Xcia);                                                                   
        Pcia = %Xlate(' ':'0':Pcia);                                                                
        WfechaA = %Xlate('-':'/':%Char(%Date() - %Days(20):*Iso));                                  
        Exec Sql                                                                                    
         Declare CX Cursor For                                                                      
         Select Distinct(Rbsarc)                                                                    
         From Votrea00/Vttrbsf                                                                      
         Where Rbsinp = 'T' And Rbsarc <> ' ' And Rbscia = :Pcia                                    
               And Rbsfca >= :WfechaA;                                                              
                                                                                                    
        Exec Sql                                                                                    
         Open CX;                                                                                   
                                                                                                    
        Exec Sql                                                                                    
         Fetch CX Into :WRbsarc;                                                                    
                                                                                                    
        Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                     
         If PrChkObjFile(WRbsarc) = *Off;                                                           
          PrPrincipal(WRbsarc);                                                                     
         Endif;                                                                                     
         Exec Sql                                                                                   
          Fetch CX Into :WRbsarc;                                                                   
        Enddo;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close CX;                                                                                  
                                                                                                    
        Exec Sql                                                                                    
         Fetch CY Into :Xcia;                                                                       
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CY;                                                                                   
                                                                                                    
       PrVttrbsf();                                                                                 
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Principal del Programa Ingreso de Información      //                       
       //------------------------------------------------------------------//                       
     PPrPrincipal      B                                                                            
     DPrPrincipal      Pi                                                                           
     DPfileYY                        10a                                                            
      ** Definición de Variables de Trabajo                                                         
     DNroreg           s              5s 0 Inz(*Zeros)                                              
     DKres             s              2a   Inz('No')                                                
     DSqlNr1           s           1000a   Inz(*Blanks)                                             
     DWvari            s            200a   Inz(*Blanks)                                             
      /Free                                                                                         
        Kres = 'No';                                                                                
        SqlNr1 = *Blanks;                                                                           
        SqlNr1 = 'Select Count(0) As NroReg' +                                                      
                 ' From Qgpl/' + %Trim(PfileYY) +                                                   
                 ' For Read Only';                                                                  
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur0 From :SqlNr1;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor0 Asensitive Scroll Cursor For Cur0;                                         
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor0;                                                                              
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor0 Into :NroReg;                                                                 
                                                                                                    
        If Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
         If NroReg <= *Zeros;                                                                       
          Kres = 'Si';                                                                              
         Endif;                                                                                     
        ElseIf Sqlcod = 100;                                                                        
         Kres = 'Si';                                                                               
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor0;                                                                              
                                                                                                    
       If Kres = 'Si';                                                                              
        If PrChequeaBloqueo(WRbsarc) = *Off;                                                        
         PrBorrarRegPgsord(PfileYY);                                                                
         Wvari = *Blanks;                                                                           
         Wvari = 'Dltobj Obj(Qgpl/' + %Trim(PfileYY) + ')'  +                                       
                 ' Objtype(*File)';                                                                 
         Cmd = %Trim(Wvari);                                                                        
         Monitor;                                                                                   
          Qcmdexc(Cmd:%Size(Cmd));                                                                  
         On-Error *All;                                                                             
         Endmon;                                                                                    
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrPrincipal      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Principal del Programa Ingreso de Información      //                       
       //------------------------------------------------------------------//                       
     PPrChequeaBloqueo...                                                                           
     P                 B                                                                            
     DPrChequeaBloqueo...                                                                           
     D                 Pi             1n                                                            
     DPfileYY                        10a                                                            
      ** Definición de Variables de Trabajo                                                         
     DWind             s              1n   Inz(*Off)                                                
     DCmd              s           1000a   Inz(*Blanks)                                             
     DNro              s              5s 0 Inz(*Zeros)                                              
     DFile1            s             10a   Inz('QPDSPOLK')                                          
     DWvari            s            200a   Inz(*Blanks)                                             
      /Free                                                                                         
       PgmVerifica(File1:PfileYY);                                                                  
       Cmd = *Blanks;                                                                               
       Cmd = 'Select Count(0) ' +                                                                   
             'From Qtemp/' + %Trim(PfileYY);                                                        
       Exec Sql                                                                                     
        Prepare Cur1 From :Cmd;                                                                     
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1 Cursor For Cur1;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1 Into :Nro;                                                                    
                                                                                                    
       If Sqlcod <> 100 And SqlCod >= *Zeros;                                                       
        If Nro > 6;                                                                                 
         Wind = *On;                                                                                
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor1;                                                                              
                                                                                                    
       Wvari = *Blanks;                                                                             
       Wvari = 'Dltobj Obj(Qtemp/' + %Trim(PfileYY) + ')'  +                                        
               ' Objtype(*File)';                                                                   
       Cmd = %Trim(Wvari);                                                                          
       Monitor;                                                                                     
        Qcmdexc(Cmd:%Size(Cmd));                                                                    
       On-Error *All;                                                                               
       Endmon;                                                                                      
       Return Wind;                                                                                 
      /End-Free                                                                                     
     PPrChequeaBloqueo...                                                                           
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Registro del Archivo Pgord de la Qgpl    //                       
       //------------------------------------------------------------------//                       
     PPrBorrarRegPgsord...                                                                          
     P                 B                                                                            
     DPrBorrarRegPgsord...                                                                          
     D                 Pi                                                                           
     DPfileXX                        10a                                                            
      ** Definición de Variables de Trabajo                                                         
     DPfechaX          s             10a   Inz(*Blanks)                                             
      /Free                                                                                         
        PfechaX = %Char(%Date());                                                                   
        Exec Sql                                                                                    
         Delete From Qgpl/PgsOrd Where Trim(Wfile) = Trim(:PfileXX)                                 
         And Wpais = :Pcia And Wfecha = :PfechaX;                                                   
      /End-Free                                                                                     
     PPrBorrarRegPgsord...                                                                          
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Registro del Archivo Vttrbsf             //                       
       //------------------------------------------------------------------//                       
     PPrVttrbsf        B                                                                            
     DPrVttrbsf        Pi                                                                           
      /Free                                                                                         
        Exec Sql                                                                                    
         Delete From Votrea00/Vttrbsf                                                               
         Where Rbsarc = ' ';                                                                        
      /End-Free                                                                                     
     PPrVttrbsf        E                                                                            
                                                                                                    
       //----------------------------------------------------------------//                         
       //  Subprocedimiento Encargado de Verificar si ya Existe el       //                         
       //  Archivo Creado en la Libreria Qtemp                           //                         
       //----------------------------------------------------------------//                         
     PPrChkObjFile     B                                                                            
     DPrChkObjFile     Pi              n                                                            
     DOfile                          10a                                                            
     ** Definición de Variables de Trabajo                                                          
     DOsw              s               n   Inz(*Off)                                                
      /Free                                                                                         
       // Chequea si Existe el Archivo                                                              
       Osw = *Off;                                                                                  
       Cmd = 'CHKOBJ OBJ(' + 'Qgpl/' + %Trim(Ofile) + ')' +                                         
             ' OBJTYPE(*FILE)';                                                                     
       Callp(e) Qcmdexc(Cmd:%Size(Cmd));                                                            
       If %Error;                                                                                   
        Osw = *On;                                                                                  
       Endif;                                                                                       
       Return Osw;                                                                                  
      /End-Free                                                                                     
     PPrChkObjFile     E                                                                            
