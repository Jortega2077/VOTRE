****************************************************************************                        
     **                                                                    *                        
     ** Programa      : VTMCCC19R                                          *                        
     ** Descripciòn:  : Generar Archivo Analisis partienteo del archivo    *                        
     **                 RESTADO    ======> AESTADO                         *                        
     ** Proyecto      : Estados de Cuentas                                 *                        
     ** Autor         : John William Palacio Cardenas PersonalSoft         *                        
     ** Fecha Creaciòn: 21 de Abril de 2015                                *                        
     **                                                                    *                        
     ***********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HDftactgrp(*No)                                                                                
     HTimfmt(*Hms)                                                                                  
     HOption(*Srcstmt:*Nodebugio)                                                                   
                                                                                                    
     **Definición de archivos                                                                       
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DWsql             s           1000a   Inz(*Blanks)                                             
     DUser             S             10A   Inz(*User)                                               
     DW_Campo          S           1000A                                                            
     DW_Cupo           S             20A                                                            
     DW_Cupo1          S              9S 0                                                          
     DW_CasVlr         S             11S 2                                                          
     DW_CasPag         S             11S 2                                                          
     DW_CasSal         S             11S 2                                                          
     DW_CasVlrT        S             20A                                                            
     DW_CasPagT        S             20A                                                            
     DW_CasSalT        S             20A                                                            
     DW_Simbolo        S             10A                                                            
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DP_Cia                           3A                                                            
     DP_Libreria                     10A                                                            
     DP_Archivo                      10A                                                            
     DP_Archivo2                     10A                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DP_Cia                           3A                                                            
     DP_Libreria                     10A                                                            
     DP_Archivo                      10A                                                            
     DP_Archivo2                     10A                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       Clear Wsql;                                                                                  
       Wsql = 'Delete From ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo);                         
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'Insert Into ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' values(''Compradora'', ''CIA    Cedula       Nombre'')';                            
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'Insert into ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' ( ' +                                                                               
              'select ' +                                                                           
              'tipotra, cia || ''    '' || cedula   || ''    '' ||    nombre ' +                    
              'from ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo2) +                              
              ' where tipotra = ''Compradora''  )' ;                                                
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' VALUES(''Compradora'',' +                                                           
              '''Zona      Cupo        TipComp  Clase   Tot_pedidos'')' ;                           
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'Select Cupo ' +                                                                      
              'FROM ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo2) +                              
              ' WHERE TIPOTRA = ''Compradora''';                                                    
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur0 From :Wsql;                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Declare Cur0 Cursor For Cur0;                                                               
                                                                                                    
       Exec Sql                                                                                     
        Open Cur0;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur0 Into :W_Cupo1;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Close Cur0;                                                                                 
                                                                                                    
       W_Simbolo = Prmoneda();                                                                      
       W_Cupo = %trim( W_Simbolo ) + ' ' +                                                          
                      %trim(%Editc (W_Cupo1 :'N' ) );                                               
                                                                                                    
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' ( ' +                                                                               
              ' SELECT ' +                                                                          
              'TIPOTRA,  ZONA || '' '' || ' + '''' + %Trim(W_Cupo) + '''' +                         
              ' || ''    '' || TIPCOMP  || ' +                                                      
              '''           '' || CLACOMP || ''        '' ||  char(TOTPED) ' +                      
              'FROM ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo2) +                              
              ' WHERE TIPOTRA = ''Compradora'')';                                                   
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' VALUES( ' +                                                                         
              ''' '', '' '')';                                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' VALUES( ' +                                                                         
              '''PagoAnticipado'', ' +                                                              
              '''Cantida Pedidos  Fec.Factura  Fec.Vecto  '')';                                     
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' ( ' +                                                                               
              'SELECT  ''PagoAnticipado'' ,  '  +                                                   
              '  NROPED ||''                 ''||' +                                                
              'CHAR(FECFAC)     ||''     ''||   CHAR(FECVTO) ' +                                    
              'FROM ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo2) +                              
              ' WHERE TIPOTRA =  ''PagoAnticipado'')';                                              
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' VALUES( ' +                                                                         
              ''' '', '' '')';                                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       WSql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' VALUES(' +                                                                          
              '''PagoCredito'', ' +                                                                 
              '''Cantida Pedidos  Fec.Factura  Fec.Vecto   Campaña ABG  Dias'+                      
              '_Mora  Promedio Dias Pago '')';                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' ( ' +                                                                               
              'SELECT ''PagoCredito'', ' +                                                          
              'NROPED ||''                         ''|| CHAR(FECFAC) '+                             
              '||''      ''||CHAR(FECVTO) ||''        ''|| ' +                                      
              '  CAMABO ||''                  '''+                                                  
              '|| NRODIA ||''                     ''|| PEDPRO ' +                                   
              'FROM ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo2) +                              
              ' WHERE TIPOTRA = ''PagoCredito'')';                                                  
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' VALUES( ' +                                                                         
              ''' '', '' '')';                                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'Select ' +                                                                           
         'Trim(Char(RCARI1))||'' a ''||Trim(char( RCARF1)) ||'' dias| '' ||'+                       
         'Trim(Char(RCARI2))||'' a ''||Trim(char( RCARF2)) ||'' dias| '' ||'+                       
         'Trim(Char(RCARI3))||'' a ''||Trim(char( RCARF3)) ||'' dias| '' ||'+                       
         'Trim(Char(RCARI4))||'' a ''||Trim(char( RCARF4)) ||'' dias| '' ||'+                       
         'Trim(Char(RCARI5))||'' a ''||Trim(char( RCARF5)) ||'' dias| '' ||'+                       
         'Trim(Char(RCARI6))||'' a ''||Trim(char( RCARF6)) ||'' dias| '' ||'+                       
         'Trim(Char(RCARI7))||'' a ''||Trim(char( RCARF7)) ||'' dias| '' ||'+                       
         'Trim(Char(RCARI8))||'' a ''||Trim(char( RCARF8)) ||'' dias| '' ||'+                       
         'Trim(Char(RCARI9))||'' a ''||Trim(char( RCARF9)) ||'' dias| ''   '+                       
              'From VOTREA00/VTMRCAF ' +                                                            
              'Where RcaCia =' + '''' + %trim(P_Cia) + '''';                                        
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur1 From :Wsql;                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Declare Cur1 Cursor For Cur1;                                                               
                                                                                                    
       Exec Sql                                                                                     
        Open Cur1;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur1 Into :W_Campo;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Close Cur1;                                                                                 
                                                                                                    
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' VALUES( ' +                                                                         
              '''CarteraEdades'', ' + '''' + %Trim(W_Campo) + '''' + ')';                           
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              '  ( ' +                                                                              
              'SELECT ''CarteraEdades'', ' +                                                        
              'RANGO1 || ''|''' +                                                                   
              '|| RANGO2 || ''|''' +                                                                
              '|| RANGO3 || ''|''' +                                                                
              '|| RANGO4 || ''|''' +                                                                
              '|| RANGO5 || ''|''' +                                                                
              '|| RANGO6 || ''|''' +                                                                
              '|| RANGO7 || ''|''' +                                                                
              '|| RANGO8 || ''|''' +                                                                
              '|| RANGO9 || ''|''' +                                                                
              'FROM ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo2) +                              
              ' WHERE TIPOTRA = ''CarteraEdades'')';                                                
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'Select ' +                                                                           
         '''% ''||Trim(Char(RCARI1))||'' a ''||' +                                                  
         'Trim(char( RCARF1)) ||'' dias| '' ||' +                                                   
         '''% ''||Trim(Char(RCARI2))||'' a ''||' +                                                  
         'Trim(char( RCARF2)) ||'' dias| '' ||' +                                                   
         '''% ''||Trim(Char(RCARI3))||'' a ''||' +                                                  
         'Trim(char( RCARF3)) ||'' dias| '' ||' +                                                   
         '''% ''||Trim(Char(RCARI4))||'' a ''||' +                                                  
         'Trim(char( RCARF4)) ||'' dias| '' ||' +                                                   
         '''% ''||Trim(Char(RCARI5))||'' a ''||' +                                                  
         'Trim(char( RCARF5)) ||'' dias| '' ||' +                                                   
         '''% ''||Trim(Char(RCARI6))||'' a ''||' +                                                  
         'Trim(char( RCARF6)) ||'' dias| '' ||' +                                                   
         '''% ''||Trim(Char(RCARI7))||'' a ''||' +                                                  
         'Trim(char( RCARF7)) ||'' dias| '' ||' +                                                   
         '''% ''||Trim(Char(RCARI8))||'' a ''||' +                                                  
         'Trim(char( RCARF8)) ||'' dias| '' ||' +                                                   
         '''% ''||Trim(Char(RCARI9))||'' a ''||' +                                                  
         'Trim(char( RCARF9)) ||'' dias| ''' +                                                      
              'From VOTREA00/VTMRCAF ' +                                                            
              'Where RcaCia =' + '''' + %trim(P_Cia) + '''';                                        
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur2 From :Wsql;                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Declare Cur2 Cursor For Cur2;                                                               
                                                                                                    
       Exec Sql                                                                                     
        Open Cur2;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur2 Into :W_Campo;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Close Cur2;                                                                                 
                                                                                                    
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' VALUES( ' +                                                                         
              '''CarteraEdades'', ' + '''' + %Trim(W_Campo) + '''' + ')';                           
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              '  ( ' +                                                                              
              'SELECT ''%CarteraEdades'',  PORRA1 || ''|''' +                                       
              '|| PORRA2 || ''|''' +                                                                
              '|| PORRA3 || ''|''' +                                                                
              '|| PORRA4 || ''|''' +                                                                
              '|| PORRA5 || ''|''' +                                                                
              '|| PORRA6 || ''|''' +                                                                
              '|| PORRA7 || ''|''' +                                                                
              '|| PORRA8 || ''|''' +                                                                
              '|| PORRA9 || ''|''' +                                                                
              'FROM ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo2) +                              
              ' WHERE TIPOTRA = ''%CarteraEdades'')';                                               
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' VALUES( ' +                                                                         
              ''' '', '' '')';                                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' VALUES( ' +                                                                         
              '''Castigo/Pago'', ' +                                                                
              '''Nro_Castg  Valor_Cast Valor_PagCast  Valor_Saldo '')';                             
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'Select CasVlr, CasPag, CasSal ' +                                                    
              'FROM ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo2) +                              
              ' Where TipoTra Like ''Castigo%''';                                                   
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur3 From :Wsql;                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Declare Cur3 Cursor For Cur3;                                                               
                                                                                                    
       Exec Sql                                                                                     
        Open Cur3;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur3 Into :W_CasVlr, :W_CasPag, :W_CasSal;                                            
                                                                                                    
       Exec Sql                                                                                     
        Close Cur3;                                                                                 
                                                                                                    
       W_CasVlrT = %trim( W_Simbolo ) + ' ' +                                                       
                      %trim(%Editc (W_CasVlr :'N' ) );                                              
                                                                                                    
       W_CasPagT = %trim( W_Simbolo ) + ' ' +                                                       
                      %trim(%Editc (W_CasPag :'N' ) );                                              
                                                                                                    
       W_CasSalT = %trim( W_Simbolo ) + ' ' +                                                       
                      %trim(%Editc (W_CasSal :'N' ) );                                              
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' (' +                                                                                
              'SELECT ''Castigo/Pago'', ' +                                                         
              'NROPED || ''           '' || ' +                                                     
              '''' + W_CasVlrt + '''' + '|| ''        '' ||' +                                      
              '''' + W_CASPAGt + '''' + '|| ''        '' ||' +                                      
              '''' + W_CASSALt + '''' +                                                             
              'FROM ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo2) +                              
              ' WHERE TIPOTRA LIKE  ''Castigo%'')';                                                 
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' VALUES( ' +                                                                         
              ''' '', '' '')';                                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' VALUES( ' +                                                                         
              '''EstudioCredito'', ' +                                                              
              '''Aprobado   Negado   Condición  #Pedidos PagAntc Cupo Asignado'+                    
                '  Comentario '')';                                                                 
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'Select CupOas ' +                                                                    
              'FROM ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo2) +                              
              ' WHERE TIPOTRA LIKE ''EstudioCredito%''';                                            
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur4 From :Wsql;                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Declare Cur4 Cursor For Cur4;                                                               
                                                                                                    
       Exec Sql                                                                                     
        Open Cur4;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur4 Into :W_Cupo1;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Close Cur4;                                                                                 
                                                                                                    
       W_Cupo = %trim( W_Simbolo ) + ' ' +                                                          
                      %trim(%Editc (W_Cupo1 :'N' ) );                                               
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              '  (' +                                                                               
              'SELECT ''EstudioCredito'',    ' +                                                    
              'APROBA || ''          '' || ' +                                                      
              'NEGADO || ''                           ''    || ' +                                  
              'CONDIC || ''                '' || ' +                                                
              'NPEDCO  || ''                                ''' +                                   
              '||' + '''' + %Trim(W_Cupo) + '''' + '|| ' +                                          
              '''             ''||  OBSERV ' +                                                      
              'from ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo2) +                              
              ' WHERE TIPOTRA LIKE ''EstudioCredito%'')';                                           
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              ' VALUES( ' +                                                                         
              ''' '', '' '')';                                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                        
              '  (' +                                                                               
              'SELECT ''Politica Aplicada'',    ' +                                                 
              'OBSERV ' +                                                                           
              'from ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo2) +                              
              ' WHERE TIPOTRA LIKE ''%Politica Aplicada%'')';                                       
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
      //-------------------------------------------------------------//                             
      //Recupera el Símbolo de Moneda de Acuerdo al País             //                             
      //-------------------------------------------------------------//                             
     PPrMoneda         B                                                                            
     DPrMoneda         Pi            10a                                                            
      // Definiciòn de Variables de Trabajo                                                         
     DSimboloRetorno   S             10a   Inz('$')                                                 
      /Free                                                                                         
        Exec SQL                                                                                    
          Select Smpsig Into :SimboloRetorno                                                        
          From Votrea00/Vtmsmpf                                                                     
          Where Trim(Smpcia) = Int(:P_Cia);                                                         
                                                                                                    
         Return SimboloRetorno;                                                                     
      /End-Free                                                                                     
     PPrMoneda         E                                                                            
