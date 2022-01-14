****************************************************************************                        
     **                                                                    *                        
     ** Programa      : VTMCCC10R                                          *                        
     ** Descripciòn:  : Calcula el valor de la cartera por concepto        *                        
     ** Proyecto      : Estados de Cuentas                                 *                        
     ** Autor         : John William Palacio Cardenas PersonalSoft         *                        
     ** Fecha Creaciòn: 09 de Marzo de 2015                                *                        
     **                                                                    *                        
     **********************************************************************                         
     ** Modificacion  : Se Adiciona carga de libreria 001 colombia en la   *                        
     **                 Rutina PrConPag                                    *                        
     **                 e identificarase que la compañia es 002 y la Zona  *                        
     **                 es mayor a 990                                     *                        
     **                 Seleccione la información de la compañia 001       *                        
     **                 que es donde quedan los pagos de los castigos      *                        
     ** Proyecto      : Estados de Cuentas                                 *                        
     ** Autor         : Eduardo Adrian Cordoba Montoya - Personalsoft S.A  *                        
     ** Fecha Creación: 13 de Septiembre de 2019                           *                        
     ***********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HDftactgrp(*No)                                                                                
     HTimfmt(*Hms)                                                                                  
     HOption(*Srcstmt:*Nodebugio)                                                                   
                                                                                                    
     **Definición de archivos                                                                       
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DWsql             s           1000a   Inz(*Blanks)                                             
     D$COMA            C                   ''''                                                     
     DW_Val1           S             15  0                                                          
     DW_Val2           S             15  0                                                          
     DW_Val3           S             15  0                                                          
     DW_Dia1           S              2  0                                                          
     DW_Dia2           S              2  0                                                          
     DW_Dia3           S              2  0                                                          
     DW_Orden1         S              9  0                                                          
     DW_Orden2         S              9  0                                                          
     DW_Orden3         S              9  0                                                          
     DW_Fecha1         S              8  0                                                          
     DW_Fecha2         S              8  0                                                          
     DW_Fecha3         S              8  0                                                          
     DW_Secue1         S             11  0                                                          
     DW_Secue2         S             11  0                                                          
     DW_Secue3         S             11  0                                                          
     DW_Comen1         S             20A                                                            
     DW_Comen2         S             20A                                                            
     DW_Comen3         S             20A                                                            
     DW_MLZIPC         s             10A                                                            
     DW_Archivo        s             21A                                                            
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DP_Cia                           3A                                                            
     DP_Ced                           9  0                                                          
     DP_Con                           2  0                                                          
     DP_Ord                           9  0                                                          
     DP_Val                          15  0                                                          
     DP_Fec                           8  0                                                          
     DP_Dia                           2  0                                                          
     DP_Sec                          11  0                                                          
     DP_Com                          20A                                                            
     DP_Acc                           1A                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DP_Cia                           3A                                                            
     DP_Ced                           9  0                                                          
     DP_Con                           2  0                                                          
     DP_Ord                           9  0                                                          
     DP_Val                          15  0                                                          
     DP_Fec                           8  0                                                          
     DP_Dia                           2  0                                                          
     DP_Sec                          11  0                                                          
     DP_Com                          20A                                                            
     DP_Acc                           1A                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
                                                                                                    
       // Recupera Zona para la cedula que ingresa                                                  
       Wsql = ' Select MLZIPC  ' +                                                                  
              ' From Opf' +  %Trim(P_Cia) + '/LMLLST01' +                                           
              ' Where MLCSTÑ = ' + %Char(P_Ced) +                                                   
              ' Fetch First 1 rows only ';                                                          
                                                                                                    
       Exec Sql  Prepare C17 From :Wsql;                                                            
       Exec Sql  Declare C12 Scroll Cursor For C17;                                                 
       Exec Sql  Open C12;                                                                          
       Exec Sql  fetch C12 Into :W_MLZIPC;                                                          
       Exec Sql  Close C12;                                                                         
                                                                                                    
       Select;                                                                                      
         When P_Acc = 'C';                                                                          
           PrConCas();                                                                              
         When P_Acc = 'P';                                                                          
           PrConPag();                                                                              
       EndSl;                                                                                       
                                                                                                    
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si tiene castigo                      //                       
       //------------------------------------------------------------------//                       
     PPrConCas         B                                                                            
     DPrConCas         Pi                                                                           
      /Free                                                                                         
                                                                                                    
       W_Archivo =' Opf' + %Trim(P_Cia) + '/pardtl ';                                               
                                                                                                    
       // Solo si Cia es Colombia y zona es mayor a 990 se                                          
       // ya quie los pagos de las castigadas de la 998  se generan en  001                         
       If P_Cia = '001' And W_MLZIPC > '990';                                                       
          W_Archivo = 'Votrea001/VLARDTL01';                                                        
       Endif;                                                                                       
       Wsql = 'Select Max(AdordÑ), sum(adtram) Valor, ' +                                           
              'Max((Digits(AdtRcc))||(Digits(AdtRyy))||(Digits(AdtRmm))||' +                        
              '(Digits(AdtRdd))), Count(*) Dias_mora, Max(AdsEqÑ), ' +                              
              'Max(AdtNdÑ) ' +                                                                      
              //'From opf' + %Trim(P_Cia) + '/pardtl ' +                                            
              'From ' + %Trim(W_Archivo) + ' ' +                                                    
              'Where ADCSTÑ = ' + %Char(P_Ced) +                                                    
              ' And AdrDpt In (' + %Char(P_Con) + ') ';                                             
                                                                                                    
       Exec Sql                                                                                     
         Prepare C1 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C1 Scroll Cursor For C1;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C1;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C1 Into :W_Orden1, :W_Val1, :W_Fecha1, :W_Dia1,                                      
                       :W_Secue1, :W_Comen1;                                                        
                                                                                                    
       Wsql = 'Select Max(AdordÑ), sum(adtram) Valor, ' +                                           
              'Max((Digits(AdtRcc))||(Digits(AdtRyy))||(Digits(AdtRmm))||' +                        
              '(Digits(AdtRdd))), Count(*) Dias_mora, Max(AdsEqÑ), ' +                              
              'Max(AdtNdÑ) ' +                                                                      
              'From Votrea' + %Trim(P_Cia) + '/pardtlHis ' +                                        
              'Where ADCSTÑ = ' + %Char(P_Ced) +                                                    
              ' And AdrDpt In (' + %Char(P_Con) + ') ' ;                                            
                                                                                                    
       Exec Sql                                                                                     
         Prepare C2 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C2 Scroll Cursor For C2;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C2;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
       fetch C2 Into :W_Orden2, :W_Val2, :W_Fecha2, :W_Dia1,                                        
                     :W_Secue2, :W_Comen2;                                                          
                                                                                                    
       Wsql = 'Select Max(AdordÑ), sum(adtram) Valor, ' +                                           
              'Max((Digits(AdtRcc))||(Digits(AdtRyy))||(Digits(AdtRmm))||' +                        
              '(Digits(AdtRdd))), Count(*) Dias_mora, Max(AdsEqÑ), ' +                              
              'Max(AdtNdÑ) ' +                                                                      
              'From Votrea' + %Trim(P_Cia) + '/Vthparf ' +                                          
              'Where ADCSTÑ = ' + %Char(P_Ced) +                                                    
              ' And AdrDpt In (' + %Char(P_Con) + ') ';                                             
                                                                                                    
       Exec Sql                                                                                     
         Prepare C3 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C3 Scroll Cursor For C3;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C3;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
       fetch C3 Into :W_Orden3, :W_Val3, :W_Fecha3, :W_Dia1,                                        
                     :W_Secue3, :W_Comen3;                                                          
                                                                                                    
       If W_Orden1 > W_Orden2 And W_Orden1 > W_Orden3;                                              
         P_ord = W_Orden1;                                                                          
         P_Com = W_Comen1;                                                                          
         P_Fec = W_Fecha1;                                                                          
         P_Sec = W_Secue1;                                                                          
       ElseIf W_Orden2 > W_Orden1 And W_Orden2 > W_Orden3;                                          
         P_ord = W_Orden2;                                                                          
         P_Com = W_Comen2;                                                                          
         P_Fec = W_Fecha2;                                                                          
         P_Sec = W_Secue2;                                                                          
       Else;                                                                                        
         P_ord = W_Orden3;                                                                          
         P_Com = W_Comen3;                                                                          
         P_Fec = W_Fecha3;                                                                          
         P_Sec = W_Secue3;                                                                          
       EndIf;                                                                                       
       P_Val = W_Val1 + W_Val2 + W_Val3;                                                            
       P_Dia = W_Dia1 + W_Dia2 + W_Dia3;                                                            
                                                                                                    
       Exec Sql                                                                                     
         Close C1;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
         Close C2;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
         Close C3;                                                                                  
                                                                                                    
      /End-Free                                                                                     
     PPrConCas         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si tiene valor pagado para el castigo //                       
       //------------------------------------------------------------------//                       
     PPrConPag         B                                                                            
     DPrConPag         Pi                                                                           
      /Free                                                                                         
                                                                                                    
                                                                                                    
       W_Archivo =' Opf' + %Trim(P_Cia) + '/pardtl ';                                               
                                                                                                    
       // Solo si Cia es Colombia y zona es mayor a 990 se cambia 002 x 001                         
       // ya quie los pagos de las castigadas de la 998  se generan en  001                         
       If P_Cia = '001' And W_MLZIPC > '990';                                                       
          W_Archivo = 'Votrea001/VLARDTL01';                                                        
       Endif;                                                                                       
       Wsql = 'Select sum(adtram) Valor ' +                                                         
              //'From opf' + %Trim(P_Cia) + '/pardtl ' +                                            
              'From ' + %Trim(W_Archivo) + ' ' +                                                    
              'Where adordÑ = ' + %Char(P_Ced) +                                                    
              //' And AdrDpt In (' + $Coma + 'P' + $Coma +                                          
              ' And AdTrty = ' + $Coma + 'P' + $Coma +                                              
              ' And AdcStÑ in (11000000, 11000001, 201499990, 11000002)  ';                         
                                                                                                    
       Exec Sql                                                                                     
         Prepare C4 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C4 Scroll Cursor For C4;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C4;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C4 Into :W_Val1;                                                                     
                                                                                                    
       Wsql = 'Select sum(adtram) Valor ' +                                                         
              'From Votrea' + %Trim(P_Cia) + '/pardtlHis ' +                                        
              'Where adordÑ = ' + %Char(P_Ced) +                                                    
              ' And AdcStÑ in (11000000, 11000001, 201499990, 11000002)  ' +                        
              ' And AdTrty = ' + $Coma + 'P' + $Coma;                                               
              //' And AdrDpt In (' + %Char(P_Con) + ')';                                            
                                                                                                    
       Exec Sql                                                                                     
         Prepare C5 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C5 Scroll Cursor For C5;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C5;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C5 Into :W_Val2;                                                                     
                                                                                                    
       Wsql = 'Select sum(adtram) Valor ' +                                                         
              'From Votrea' + %Trim(P_Cia) + '/Vthparf ' +                                          
              'Where adordÑ = ' + %Char(P_Ced) +                                                    
              ' And AdcStÑ in (11000000, 11000001, 201499990, 11000002)  ' +                        
              ' And AdTrty = ' + $Coma + 'P' + $Coma ;                                              
              //' And AdrDpt In (' + %Char(P_Con) + ')';                                            
                                                                                                    
       Exec Sql                                                                                     
         Prepare C6 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C6 Scroll Cursor For C6;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C6;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C6 Into :W_Val3;                                                                     
                                                                                                    
       P_Val = W_Val1 + W_Val2 + W_Val3;                                                            
                                                                                                    
       Exec sql                                                                                     
         Close C4;                                                                                  
                                                                                                    
       Exec sql                                                                                     
         Close C5;                                                                                  
                                                                                                    
       Exec sql                                                                                     
         Close C6;                                                                                  
                                                                                                    
      /End-Free                                                                                     
     PPrConPag         E                                                                            
