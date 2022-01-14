      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCCPR                                          **                        
      * Descripción   : Borra Archivos de Bancos ya Trabajados            **                        
      * Proyecto      : Menú de Opciones Opi Paises                       **                        
      * Autor         : (Jortega)                                         **                        
      * Fecha Creación: 23 de Febrero de 2010                             **                        
      *                                                                   **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HDftactgrp(*No)                                                                                
     HBnddir('QC2LE')                                                                               
                                                                                                    
     **Definiciòn de Archivos de Trabajo                                                            
     FVttrbsf   If   e           k Disk                                                             
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DCmd              s           3000a                                                            
     DLen              s             15p 5                                                          
     DWfecha           s             10d                                                            
     DWfechaA          s             10a   Inz(*Blanks)                                             
     DIdx              s              2s 0 Inz(1)                                                   
     DWRbsarc          s             10a   Inz(*Blanks)                                             
     DWRbsusr          s             10a   Inz(*Blanks)                                             
     DWRbsnba          s             40a   Inz(*Blanks)                                             
     DWRbsfca          s             10a   Inz(*Blanks)                                             
     DWRbsinp          s              1a   Inz(*Blanks)                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DPrPrincipal      Pr                                                                           
     DPfileYY                        10a                                                            
     DPrChequeaBloqueo...                                                                           
     D                 Pr             1n                                                            
     DPfileYY                        10a                                                            
                                                                                                    
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
                                                                                                    
     DPgmVerifica      Pr                  Extpgm('VOTREP00/VTMCCCVL')                              
     DFsPool                         10a                                                            
     DFfile                          10a                                                            
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       WfechaA = %Xlate('-':'/':%Char(%Date() - %Days(5):*Iso));                                    
       Exec Sql                                                                                     
        Declare CX Cursor For                                                                       
        Select Distinct(Rbsarc), Rbsusr, Rbsnba, Rbsfca, Rbsinp                                     
        From Vttrbsf                                                                                
        Where Rbsinp = 'T' And Rbsarc <> ' ' And Rbscia = :Pcia                                     
              And Rbsfca >= :WfechaA;                                                               
                                                                                                    
       Exec Sql                                                                                     
        Open CX;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch CX Into :WRbsarc, :WRbsusr, :WRbsnba, :WRbsfca, :WRbsinp;                             
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        PrPrincipal(WRbsarc);                                                                       
        Exec Sql                                                                                    
         Fetch CX Into :WRbsarc, :WRbsusr, :WRbsnba, :WRbsfca, :WRbsinp;                            
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CX;                                                                                   
                                                                                                    
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
     DCmd              s           3000a   Inz(*Blanks)                                             
      /Free                                                                                         
        Kres = 'No';                                                                                
        SqlNr1 = *Blanks;                                                                           
        SqlNr1 = 'Select Count(0) As NroReg' +                                                      
                 ' From Qgpl/' + %Trim(PfileYY) +                                                   
                 ' For Read Only';                                                                  
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur0 From :SqlNr1;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor0 Cursor For Cur0;                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor0;                                                                              
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor0 Into :NroReg;                                                                 
                                                                                                    
        If Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
         If NroReg <= *Zeros;                                                                       
          Kres = 'Si';                                                                              
         Else;                                                                                      
          If WRbsinp = 'P';                                                                         
          Cmd = *Blanks;                                                                            
          Cmd = 'Update Votrea00/Vttrbsf Set ' +                                                    
                'Rbsinp = ''P''' + ', Rbstre = 0, ' +                                               
               'Rbsvto = 0, Rbsarc = ' + '''' + %Trim(PfileYY) + '''' +' ' +                        
               'Where Rbscia = ' + '''' + Pcia + '''' + ' And ' +                                   
               'Trim(Rbsnba) = ' + '''' + %Trim(WRbsnba) + '''' + ' And ' +                         
               'Trim(Rbsusr) = ' + '''' + %Trim(WRbsusr) + '''' + ' And ' +                         
               'Rbsfca= ' + '''' + %Trim(WRbsfca) + '''';                                           
                                                                                                    
          Exec Sql                                                                                  
           Prepare Cur From :Cmd;                                                                   
                                                                                                    
          Exec Sql                                                                                  
           Execute Cur;                                                                             
         Endif;                                                                                     
        Endif;                                                                                      
        ElseIf Sqlcod = 100;                                                                        
         If NroReg <= *Zeros;                                                                       
          Kres = 'Si';                                                                              
         Endif;                                                                                     
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor0;                                                                              
                                                                                                    
       If Kres = 'Si';                                                                              
        If PrChequeaBloqueo(WRbsarc) = *Off;                                                        
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
