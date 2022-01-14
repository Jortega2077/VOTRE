****************************************************************************                        
     **                                                                    *                        
     ** Programa      : VTMCCC17R                                          *                        
     ** Descripciòn:  : PGM Crea indice basado en el archivo que entra     *                        
     **                 Estado  RESTADO                                    *                        
     ** Proyecto      : Estados de Cuentas                                 *                        
     ** Autor         : Martha C. Restrepo                                 *                        
     ** Fecha Creaciòn: 16 de Julio de 2015                                *                        
     **                                                                    *                        
     ***********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HDftactgrp(*No)                                                                                
     HTimfmt(*Hms)                                                                                  
     HOption(*Srcstmt:*Nodebugio)                                                                   
                                                                                                    
     **Definición de archivos                                                                       
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     Dwsql             s           5000a   Inz(*Blanks)                                             
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DP_Libreria                     10A                                                            
     DP_Archivo                      10A                                                            
     DP_Indice                       10A                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DP_Libreria                     10A                                                            
     DP_Archivo                      10A                                                            
     DP_Indice                       10A                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
                                                                                                    
         wSql = *BLANKS;                                                                            
         wSql = 'Create INDEX ' + %trim(P_Libreria) + '/' + %trim(P_Indice) +                       
              ' ON ' + %trim(P_Libreria) + '/' + %trim(P_Archivo) +                                 
              ' (SEQ)';                                                                             
                                                                                                    
         Exec Sql Execute Immediate :wSql;                                                          
                                                                                                    
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
