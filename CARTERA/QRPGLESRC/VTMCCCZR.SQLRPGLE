***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCCZR                                          *                         
     ** Descripciòn:  : Borra Registros de Pagos de Bancos ya Procesados  *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 12 de Julio de 2011                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
     HDebug(*Yes)                                                                                   
     HBnddir('QC2LE')                                                                               
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DSqlStr           s            500a   Inz(*Blanks)                                             
     DCmd              s           3000a   Inz(*Blanks)                                             
     DLen              s             15p 5                                                          
     DObjName          s             10a   Inz(*Blanks)                                             
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DEstruFilesW      Ds                                                                           
     DConsecW                         9s 0                                                          
     DCompaÑiaW                       3a                                                            
     DCedulaW                       200a                                                            
                                                                                                    
     DEstruFiles       Ds                                                                           
     DWconsec                         9s 0                                                          
     DWpais                           3a                                                            
     DWfile                          10a                                                            
     DWfecha                         10a                                                            
     DCedula                        200a                                                            
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DPrPgsord         Pr                                                                           
     DPrVerificarFile  Pr                                                                           
     DPrConsultarRegFile...                                                                         
     D                 Pr                                                                           
     DPrBorrarRegFile  Pr                                                                           
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
        PrPgsord();                                                                                 
        *Inlr = *On;                                                                                
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Lee Archivo Pgsord                             //                       
       //------------------------------------------------------------------//                       
     PPrPgsord         B                                                                            
     DPrPgsord         Pi                                                                           
      /Free                                                                                         
       // Arma la Estructura Requerida del Sql                                                      
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Wconsec, Wpais, Wfile, Wfecha, Cedula ' +                                   
                'From Qgpl/Pgsord';                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur0 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor0 Cursor For Cur0;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor0;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor0 Into :EstruFiles;                                                             
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        PrVerificarFile();                                                                          
        Exec Sql                                                                                    
         Fetch Cursor0 Into :EstruFiles;                                                            
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor0;                                                                              
                                                                                                    
      /End-Free                                                                                     
     PPrPgsord         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Verifica si el Archivo Existe                  //                       
       //------------------------------------------------------------------//                       
     PPrVerificarFile  B                                                                            
     DPrVerificarFile  Pi                                                                           
      /Free                                                                                         
       // Chequea si Existe el Archivo                                                              
       Cmd = 'CHKOBJ OBJ(' + 'QGPL' + '/'  + %Trim(Wfile) + ')' +                                   
             ' OBJTYPE(*FILE)';                                                                     
       Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                            
       If %Error;                                                                                   
        // No Existe el Archivo                                                                     
        PrBorrarRegFile();                                                                          
       ElseIf Not %Error;                                                                           
        PrConsultarRegFile();                                                                       
       Endif;                                                                                       
                                                                                                    
      /End-Free                                                                                     
     PPrVerificarFile  E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Registro del Archivo Pgsord              //                       
       //------------------------------------------------------------------//                       
     PPrBorrarRegFile  B                                                                            
     DPrBorrarRegFile  Pi                                                                           
      /Free                                                                                         
       // Arma la Estructura Requerida del Sql                                                      
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Delete From Qgpl/Pgsord ' +                                                        
                'Where Wconsec = ' + %Trim(%Editc(Wconsec:'Z')) + ' And ' +                         
                'Wpais = ' + '''' + Wpais + '''' + ' And ' +                                        
                'Trim(Cedula) = ' + '''' + %Trim(Cedula) + '''';                                    
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
      /End-Free                                                                                     
     PPrBorrarRegFile  E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si el Registro Realmente Existe       //                       
       // en el Archivo Pgsord                                             //                       
       //------------------------------------------------------------------//                       
     PPrConsultarRegFile...                                                                         
     P                 B                                                                            
     DPrConsultarRegFile...                                                                         
     D                 Pi                                                                           
      /Free                                                                                         
       // Arma la Estructura Requerida del Sql                                                      
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Consec, CompaÑia, Cedula ' +                                                
                'From Qgpl/' + %Trim(Wfile) + ' ' +                                                 
                'Where Consec = ' + %Trim(%Editc(Wconsec:'Z')) + ' And ' +                          
                'CompaÑia = ' + '''' + Wpais + '''' + ' And ' +                                     
                'Trim(Cedula) = ' + '''' + %Trim(Cedula) + '''';                                    
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur1 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1 Cursor For Cur1;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1 Into :EstruFilesW;                                                            
                                                                                                    
       If SqlCod = 100;                                                                             
        PrBorrarRegFile();                                                                          
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor1;                                                                              
                                                                                                    
      /End-Free                                                                                     
     PPrConsultarRegFile...                                                                         
     P                 E                                                                            
