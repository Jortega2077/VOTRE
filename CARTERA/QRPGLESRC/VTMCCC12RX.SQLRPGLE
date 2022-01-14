****************************************************************************                        
     **                                                                    *                        
     ** Programa      : VTMCCC12R                                          *                        
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
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pr                  Extpgm('Main')                                           
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       Clear Wsql;                                                                                  
       Wsql = 'Delete From ' + %Trim(User) + '/AEstado';                                            
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'Insert Into ' + %Trim(User) + '/Aestado ' +                                          
              'values(''Compradora'', ''CIA    Cedula       Nombre'')';                             
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'Insert into ' + %trim(user) + '/Aestado  ( ' +                                       
              'select ' +                                                                           
              'tipotra, cia || ''    '' || cedula   || ''    '' ||    nombre ' +                    
              'from ' + %trim(user) + '/restado ' +                                                 
              'where tipotra = ''Compradora''  )' ;                                                 
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO VALUES(''Compradora'',' +                    
              '''Zona      Cupo       TipComp  Clase   Tot_pedidos'')' ;                            
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO  (' +                                        
              'SELECT ' +                                                                           
              'TIPOTRA,  ZONA || '' '' || char(CUPO) || '' '' || TIPCOMP  || ' +                    
              '''    '' || CLACOMP || ''     '' ||  char(TOTPED) ' +                                
              'FROM ' + %TRIM(USER) + '/RESTADO ' +                                                 
              'WHERE TIPOTRA = ''Compradora'')';                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO VALUES( ' +                                  
              ''' '', '' '')';                                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO VALUES( ' +                                  
              '''PagoAnticipado'', ' +                                                              
              '''Cantida Pedidos  Fec.Factura  Fec.Vecto  '')';                                     
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO  ( ' +                                       
              'SELECT  ''PagoAnticipado'' ,   NROPED ||''                ''||' +                    
              'CHAR(FECFAC)     ||''     ''||   CHAR(FECVTO) ' +                                    
              'FROM ' + %TRIM(USER) + '/RESTADO ' +                                                 
              'WHERE TIPOTRA =  ''PagoAnticipado'')';                                               
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO VALUES( ' +                                  
              ''' '', '' '')';                                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       WSql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO VALUES(' +                                   
              '''PagoCredito'', ' +                                                                 
              '''Cantida Pedidos  Fec.Factura  Fec.Vecto   Campaña ABG  Dias'+                      
              '_Mora  Promedio Dias Pago '')';                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO  ( ' +                                       
              'SELECT ''PagoCredito'', ' +                                                          
              'NROPED ||''                ''|| CHAR(FECFAC) '+                                      
              '||''      ''||CHAR(FECVTO) ||''   ''|| CAMABO ||''         '''+                      
              '|| NRODIA ||''           ''|| PEDPRO ' +                                             
              'FROM ' + %TRIM(USER) + '/RESTADO ' +                                                 
              'WHERE TIPOTRA = ''PagoCredito'')';                                                   
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO VALUES( ' +                                  
              ''' '', '' '')';                                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO VALUES( ' +                                  
              '''CarteraEdades'', ' +                                                               
              '''RANGO1  RANGO2  RANGO3  RANGO4  RANGO5  RANGO6 ' +                                 
              'RANGO7  RANGO8  RANGO9 '')';                                                         
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO  ( ' +                                       
              'SELECT ''CarteraEdades'',  RANGO1 || ''       ''' +                                  
              '|| RANGO2 || ''       '' || RANGO3 || ''       ''' +                                 
              '|| RANGO4 || ''       '' || RANGO5 || ''       ''' +                                 
              '|| RANGO6 || ''       '' || RANGO7 || ''      ''' +                                  
              '|| RANGO8 || ''       '' || RANGO9 ' +                                               
              'FROM ' + %TRIM(USER) + '/RESTADO ' +                                                 
              'WHERE TIPOTRA = ''CarteraEdades'')';                                                 
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO VALUES( ' +                                  
              '''CarteraEdades'', ' +                                                               
              '''RANGO1  RANGO2  RANGO3  RANGO4  RANGO5  RANGO6 ' +                                 
              'RANGO7  RANGO8  RANGO9 '')';                                                         
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO  ( ' +                                       
              'SELECT ''%CarteraEdades'',  PORRA1 || ''     ''' +                                   
              '|| PORRA2 || ''     '' || PORRA3 || ''     ''' +                                     
              '|| PORRA4 || ''    '' || PORRA5 || ''      ''' +                                     
              '|| PORRA6 || ''    '' || PORRA7 || ''     ''' +                                      
              '|| PORRA8 || ''     '' || PORRA9 ' +                                                 
              'FROM ' + %TRIM(USER) + '/RESTADO ' +                                                 
              'WHERE TIPOTRA = ''%CarteraEdades'')';                                                
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO VALUES( ' +                                  
              ''' '', '' '')';                                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO VALUES( ' +                                  
              '''Castigo/Pago'', ' +                                                                
              '''Nro_Castg  Valor_Cast Valor_PagCast  Valor_Saldo '')';                             
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO  (' +                                        
              'SELECT ''Castigo/Pago'', ' +                                                         
              'NROPED || ''           '' || CASVLR || ''        '' || ' +                           
              'CASPAG || ''         '' || CASSAL ' +                                                
              'FROM ' + %TRIM(USER) + '/RESTADO ' +                                                 
              'WHERE TIPOTRA LIKE  ''Castigo%'')';                                                  
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO VALUES( ' +                                  
              ''' '', '' '')';                                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO VALUES( ' +                                  
              '''EstudioCredito'', ' +                                                              
              '''Aprobado   Negado   Condición  #Pedidos PagAntc Cupo Asignado'+                    
                '  Comentario '')';                                                                 
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO  (' +                                        
              'SELECT ''EstudioCredito'',    ' +                                                    
              'APROBA || ''          '' || NEGADO || ''          '' || ' +                          
              'CONDIC || ''          '' || ' +                                                      
              'NPEDCO  || ''               '' || CUPOAS || ' +                                      
              '''             ''||  OBSERV ' +                                                      
              'from ' + %trim(user) + '/restado ' +                                                 
              'WHERE TIPOTRA LIKE ''EstudioCredito%'')';                                            
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO VALUES( ' +                                  
              ''' '', '' '')';                                                                      
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'INSERT INTO ' + %TRIM(USER) + '/AESTADO  (' +                                        
              'SELECT ''Politica Aplicada'',    ' +                                                 
              'OBSERV ' +                                                                           
              'from ' + %trim(user) + '/restado ' +                                                 
              'WHERE TIPOTRA LIKE ''%Politica Aplicada%'')';                                        
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
