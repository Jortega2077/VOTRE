      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCB2R                                          **                        
      * Descripción   : Programa que Genera Consulta de Pedidos Enviados  **                        
      * Proyecto      : Menu Opciones OPI Paises                          **                        
      * Autor         : (Jortega)                                         **                        
     ** Fecha Creaciòn: 18 de Septiembre de 2013                          **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtacci2p  Cf   e             Workstn                                                          
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DCmd              s            200a                                                            
     DUser             s             10a   Inz(*User)                                               
     DStrsql           s          25000a   Inz(*Blanks)                                             
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DNumero           s              9  0 Inz(*Zeros)                                              
     DValorTot         s             14  2 Inz(*Zeros)                                              
     DPtr              s               *   Inz(%Addr(*In))                                          
     DPzona            s              3a   Inz(*Blanks)                                             
     DPcampa           s              4a   Inz(*Blanks)                                             
     DPEstado          s             10a   Inz(*Blanks)                                             
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DGenerarInf                       N   Overlay(Indic:5)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DBorrarFile       Pr                                                                           
     DCrearFile        Pr                                                                           
     DInsertarFile     Pr                                                                           
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
     DPrTotales        Pr                                                                           
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     DCmd                          3000a   Options(*Varsize) Const                                  
     DCmdlen                         15p 5 Const                                                    
                                                                                                    
     DMain             Pr                  Extpgm('VTMCCB2R')                                       
     DPcia                            3a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       Clear Rdatot0;                                                                               
       Clear Mensaje1;                                                                              
       Dow Salir = *Off;                                                                            
        Write Rdatot2;                                                                              
        Exfmt Rdatot0;                                                                              
        If Salir = *On;                                                                             
         Leave;                                                                                     
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Clear Rdatot0;                                                                             
         Clear Mensaje1;                                                                            
         Iter;                                                                                      
        Endif;                                                                                      
        If GenerarInf = *On;                                                                        
         Pzona = Pzonap;                                                                            
         Pcampa = Pcampap;                                                                          
         PEstado = *Blanks;                                                                         
         If Opcion <> *Zeros;                                                                       
          If Bt1 <> *Zeros And Bt2 = *Zeros And Bt3 = *Zeros;                                       
           Pestado = '''' + 'A' + '''';                                                             
          Endif;                                                                                    
          If Bt2 <> *Zeros And Bt1 = *Zeros And Bt3 = *Zeros;                                       
           Pestado = '''' + 'C' + '''';                                                             
          Endif;                                                                                    
          If Bt3 <> *Zeros And Bt1 = *Zeros And Bt2 = *Zeros;                                       
           Pestado = '''' + 'S' + '''';                                                             
          Endif;                                                                                    
                                                                                                    
          If Bt1 <> *Zeros And Bt2 <> *Zeros And Bt3 = *Zeros;                                      
           Pestado = '''' + 'A' + '''' + ', ''S''';                                                 
          Endif;                                                                                    
          If Bt1 <> *Zeros And Bt3 <> *Zeros And Bt2 = *Zeros;                                      
           Pestado = '''' + 'A' + '''' + '''' + ', ''C''';                                          
          Endif;                                                                                    
          If Bt1 = *Zeros And Bt2 <> *Zeros And Bt3 <> *Zeros;                                      
           Pestado = '''' + 'S' + '''' + '''' + ', ''C''';                                          
          Endif;                                                                                    
                                                                                                    
          If Bt1 <> *Zeros And Bt2 <> *Zeros And Bt3 <> *Zeros;                                     
           Pestado = 'Todos';                                                                       
          Endif;                                                                                    
          PgmPais(Pcia:Xpais);                                                                      
          BorrarFile();                                                                             
          CrearFile();                                                                              
          InsertarFile();                                                                           
          PrTotales();                                                                              
          Plib = %Trim(User);                                                                       
          Parc = 'VTWPDPF';                                                                         
          Exfmt Mensaje1;                                                                           
          Cmd = 'STRQMQRY QMQRY(VOTREA00/VTWPDPFP) SETVAR((LIB ' +                                  
                 %Trim(User)  + '))';                                                               
          Qcmdexc (Cmd: %Size(Cmd));                                                                
          Clear Rdatot0;                                                                            
          Clear Mensaje1;                                                                           
         Endif;                                                                                     
        Endif;                                                                                      
       Enddo;                                                                                       
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Archivo Creado en la Qtemp               //                       
       //------------------------------------------------------------------//                       
     PBorrarFile       B                                                                            
     DBorrarFile       Pi                                                                           
      /Free                                                                                         
       Strsql = *Blanks;                                                                            
       Strsql = 'Drop Table ' + %Trim(User) + '/Vtwpdpf';                                           
                                                                                                    
       Exec Sql                                                                                     
        Execute Immediate :Strsql;                                                                  
                                                                                                    
      /End-Free                                                                                     
     PBorrarFile       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crea Archivo en la Qtemp                       //                       
       //------------------------------------------------------------------//                       
     PCrearFile        B                                                                            
     DCrearFile        Pi                                                                           
      /Free                                                                                         
       Strsql = *Blanks;                                                                            
       Strsql = 'Create Table ' + %Trim(User) + '/Vtwpdpf(' +                                       
                'Pdpzon Char(3) Ccsid 284 Default Null,' +                                          
                'Pdpord Char(9) Ccsid 284 Default Null,' +                                          
                'Pdpced Char(9) Ccsid 284 Default Null,' +                                          
                'Pdpnom Char(30) Ccsid 284 Default Null,' +                                         
                'Pdpvlr Decimal(14, 2) Default Null)';                                              
                                                                                                    
       Exec Sql                                                                                     
        Execute Immediate :Strsql;                                                                  
                                                                                                    
       Strsql = *Blanks;                                                                            
       Strsql = 'Label on Table ' + %Trim(User) + '/Vtwpdpf ' +                                     
                'Is ''Archivo Temporal Detalle de Pedidos por Campaña''';                           
                                                                                                    
       Exec Sql                                                                                     
        Execute Immediate :Strsql;                                                                  
                                                                                                    
       Strsql = *Blanks;                                                                            
       Strsql = 'Label on Column ' + %Trim(User) + '/Vtwpdpf' +                                     
                '(Pdpzon Text is ''Zona''' + ', ' +                                                 
                ' Pdpord Text is ''Orden''' + ', ' +                                                
                ' Pdpced Text is ''Cédula''' + ', ' +                                               
                ' Pdpnom Text is ''Nombre''' + ', ' +                                               
                ' Pdpvlr Text is ''Valor''' + ')';                                                  
                                                                                                    
       Exec Sql                                                                                     
        Execute Immediate :Strsql;                                                                  
                                                                                                    
      /End-Free                                                                                     
     PCrearFile        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Inserta Datos al Archivo de Trabajo            //                       
       //------------------------------------------------------------------//                       
     PInsertarFile     B                                                                            
     DInsertarFile     Pi                                                                           
      /Free                                                                                         
                                                                                                    
       Strsql = *Blanks;                                                                            
       If %Trim(Pestado) = 'Todos';                                                                 
        Strsql = 'Insert Into ' + %Trim(User) + '/Vtwpdpf ' +                                       
           '(Pdpzon, Pdpord, Pdpced, Pdpnom, Pdpvlr)' +                                             
           'Select Ohsls1, Char(OhordÑ), Char(OhcstÑ), Mlname, Ohmram ' +                           
           'From Opf' + Pcia + '/Porhdr Inner Join Opf' + Pcia + '/Lmllst01 ' +                     
           'On OhcstÑ = MlcstÑ ' +                                                                  
           'Where Ohmeda = ' + '''' + Pcampa + '''' + ' And ' +                                     
            'Ohcmfl = ''N''' + ' And ' +                                                            
           'Ohrcst In(''A''' + ', ''C''' + ', ''S''' + ') And ' +                                   
           'Ohsls1 = ' + '''' + Pzona + '''' + ' ' +                                                
           'Order By OhordÑ';                                                                       
       Else ;                                                                                       
        Strsql = 'Insert Into ' + %Trim(User) + '/Vtwpdpf ' +                                       
           '(Pdpzon, Pdpord, Pdpced, Pdpnom, Pdpvlr)' +                                             
           'Select Ohsls1, Char(OhordÑ), Char(OhcstÑ), Mlname, Ohmram ' +                           
           'From Opf' + Pcia + '/Porhdr Inner Join Opf' + Pcia + '/Lmllst01 ' +                     
           'On OhcstÑ = MlcstÑ ' +                                                                  
           'Where Ohmeda = ' + '''' + Pcampa + '''' + ' And ' +                                     
            'Ohcmfl = ''N''' + ' And ' +                                                            
           'Ohrcst In(' + %Trim(Pestado) + ') And ' +                                               
           'Ohsls1 = ' + '''' + Pzona + '''' + ' ' +                                                
           'Order By OhordÑ';                                                                       
       Endif;                                                                                       
                                                                                                    
        Exec Sql                                                                                    
         Execute Immediate :Strsql;                                                                 
                                                                                                    
      /End-Free                                                                                     
     PInsertarFile     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Genera Archivo de Totales del Archivo Temporal //                       
       //------------------------------------------------------------------//                       
     PPrTotales        B                                                                            
     DPrTotales        Pi                                                                           
      /Free                                                                                         
       Strsql = *Blanks;                                                                            
       Strsql = 'Select Count(0), Sum(Pdpvlr) ' +                                                   
                'From ' + %Trim(User) + '/Vtwpdpf';                                                 
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur0 From :Strsql;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor0 Cursor For Cur0;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor0;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor0 Into :Numero, :ValorTot;                                                      
                                                                                                    
       If Sqlcod = 100 Or Sqlcod < *Zeros;                                                          
        Numero = *Zeros;                                                                            
        ValorTot = *Zeros;                                                                          
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor0;                                                                              
                                                                                                    
        Strsql = 'Insert Into ' + %Trim(User) + '/Vtwpdpf(' +                                       
                 'Pdpzon, Pdpord, Pdpced, Pdpnom, Pdpvlr) ' +                                       
       'Values('' ''' + ', '' ''' + ', '' ''' + ', ' + '''' +                                       
               'Total de Pedidos: ' +                                                               
                 %Trim(%Editc(Numero:'P')) + '''' + ', ' +                                          
                 %Trim(%Editc(ValorTot:'P')) + ')';                                                 
                                                                                                    
        Exec Sql                                                                                    
         Execute Immediate :Strsql;                                                                 
                                                                                                    
      /End-Free                                                                                     
     PPrTotales        E                                                                            
