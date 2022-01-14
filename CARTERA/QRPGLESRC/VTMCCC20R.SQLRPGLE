****************************************************************************                        
     **                                                                    *                        
     ** Programa      : VTMCCC20R                                          *                        
     ** Descripciòn:  : Generar Archivo Resumido Registro Estudio          *                        
     **                 de Crédito                                         *                        
     ** Proyecto      : Estados de Cuentas                                 *                        
     ** Autor         : John William Palacio Cardenas PersonalSoft         *                        
     ** Fecha Creaciòn: 20 de Mayo de 2015                                 *                        
     **                                                                    *                        
     ***********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HDftactgrp(*No)                                                                                
     HTimfmt(*Hms)                                                                                  
     HOption(*Srcstmt:*Nodebugio)                                                                   
                                                                                                    
     **Definición de archivos                                                                       
     FRESTADO   If A e           K Disk                                                             
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DWsql             s           1000a   Inz(*Blanks)                                             
     DW_User           S             10A   Inz(*User)                                               
     DW_CEDULA         S              9  0                                                          
     DW_CONT           S              9  0                                                          
     DW_POCCUP         S              9  0                                                          
     DW_PEDCRE         S              5  0                                                          
     DW_PEDANT         S              5  0                                                          
     DW_PorEda         S              5  2                                                          
     DW_POCNPE         S              5  0                                                          
     DW_POPPPI         S              5  3                                                          
     DW_POPPPF         S              5  3                                                          
     DW_POPPPIF        S              5  2                                                          
     DW_POPPPFF        S              5  2                                                          
     DW_CASVLR         S             11  2                                                          
     DW_NroDia         S              4  0                                                          
     DW_IndEsc         S              1                                                             
     DW_PocRip         S              5  0                                                          
     DW_PocRfp         S              5  0                                                          
     DW_POCVCA         S             11  2                                                          
     DW_CASVLR1        S             11  2                                                          
     DW_RcdD3c         S             10A                                                            
     DW_MldM08         S              1A                                                            
     DW_MldM03         S              1A                                                            
     DW_MldM07         S              1A                                                            
     DW_MldM01         S              1A                                                            
     DW_RcdD7C         S             10A                                                            
     DW_RcdD8C         S             10A                                                            
     DW_RcdD1E         S             10A                                                            
     DW_RcdD8E         S             10A                                                            
     DW_RcdD3E         S             10A                                                            
     DW_RcdD7E         S             10A                                                            
                                                                                                    
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DP_Cia                           3A                                                            
     DP_Libreria                     10A                                                            
     DP_Archivo                      10A                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DP_Cia                           3A                                                            
     DP_Libreria                     10A                                                            
     DP_Archivo                      10A                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       Wsql = 'Select CEDULA ' +                                                                    
              'From ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                               
              '  Where TIPOTRA = ''Compradora''';                                                   
                                                                                                    
       Exec Sql                                                                                     
         Prepare C1 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C1 Scroll Cursor For C1;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C1;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C1 Into :W_Cedula;                                                                   
                                                                                                    
       Exec Sql                                                                                     
         Close C1;                                                                                  
                                                                                                    
       PrProce();                                                                                   
       PrRango();                                                                                   
       PrvalPar();                                                                                  
       PrValCas();                                                                                  
       PrDemografic();                                                                              
                                                                                                    
                                                                                                    
       If %Scan(W_MldM08:%trim(W_RcdD8c)) > *Zeros Or                                               
         %Scan(W_MldM03:%trim(W_RcdD3c)) > *Zeros Or                                                
         %Scan(W_MldM07:%trim(W_RcdD7c)) > *Zeros;                                                  
                                                                                                    
         If W_CasVlr1 > W_POCVCA;                                                                   
           PrContinuar();                                                                           
         EndIf;                                                                                     
       Else;                                                                                        
         // cambio en condición                                                                     
         // If %Scan(W_MldM01:%trim(W_RcdD1e)) > *Zeros Or                                          
         //  %Scan(W_MldM08:%trim(W_RcdD8e)) > *Zeros And                                           
         //  %Scan(W_MldM03:%trim(W_RcdD3e)) = *Zeros And                                           
         //  %Scan(W_MldM07:%trim(W_RcdD7e)) = *Zeros;                                              
         //                                                                                         
         // Cambio condición segun parámetro DEMOGRAFICO ESTUDIO CREDITO                            
           If (%Scan(W_MldM01:%trim(W_RcdD1e)) > *Zeros  And                                        
             %Scan(W_MldM03:%trim(W_RcdD3e)) = *Zeros And                                           
             %Scan(W_MldM07:%trim(W_RcdD7e)) > *Zeros )  OR                                         
              (%Scan(W_MldM08:%trim(W_RcdD8e)) > *Zeros  And                                        
             %Scan(W_MldM03:%trim(W_RcdD3e)) = *Zeros And                                           
             %Scan(W_MldM07:%trim(W_RcdD7e)) > *Zeros ) ;                                           
                                                                                                    
           PrContinuar();                                                                           
         EndIf;                                                                                     
       EndIf;                                                                                       
                                                                                                    
       If W_IndEsc = 'N';                                                                           
         Clear RegRes;                                                                              
         TIPOTRA = 'EstudioCredito';                                                                
                                                                                                    
         Wsql = 'Select CASVLR ' +                                                                  
                'From ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo)  +                            
                '  Where TIPOTRA = ''Castigo''';                                                    
                                                                                                    
         Exec Sql                                                                                   
           Prepare C7 From :Wsql;                                                                   
                                                                                                    
         Exec Sql                                                                                   
           Declare C7 Scroll Cursor For C7;                                                         
                                                                                                    
         Exec Sql                                                                                   
           Open C7;                                                                                 
                                                                                                    
         Exec Sql                                                                                   
           fetch C7 Into :W_CasVlr;                                                                 
                                                                                                    
         If SqlCode = *Zeros And W_CASVLR <> *Zeros;                                                
           Observ = 'Compradora ha tenido Castigo';                                                 
           Write REGRES;                                                                            
         Else;                                                                                      
           Wsql = 'Select (NroDia) ' +                                                              
                  'From ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                           
                  '  Where TIPOTRA = ''PagoCredito''';                                              
                                                                                                    
           Exec Sql                                                                                 
             Prepare C8 From :Wsql;                                                                 
                                                                                                    
           Exec Sql                                                                                 
             Declare C8 Scroll Cursor For C8;                                                       
                                                                                                    
           Exec Sql                                                                                 
             Open C8;                                                                               
                                                                                                    
           Exec Sql                                                                                 
             fetch C8 Into :W_NroDia;                                                               
                                                                                                    
           Exec Sql                                                                                 
             Close C8;                                                                              
           If W_NroDia > *Zeros;                                                                    
           Observ = 'Compradora con días en mora >= 1, pero aun no han sido ' +                     
                      'marcado los demográficos';                                                   
             Write REGRES;                                                                          
           EndIf;                                                                                   
         EndIf;                                                                                     
         Exec Sql                                                                                   
           Close C7;                                                                                
       EndIf;                                                                                       
                                                                                                    
       If W_PocCup <> *Zeros And W_indEsc = 'S';                                                    
         Clear RegRes;                                                                              
         Cia = %Trim(P_Cia);                                                                        
         TipoTra = 'Politica Aplicada';                                                             
         W_POPPPIF = %Dec(W_PopPpi:5:2);                                                            
         W_POPPPFF = %Dec(W_PopPpf:5:2);                                                            
         Observ = '#Pedidos_Credito ' + %Trim(%Char(W_PocRip)) + ' a ' +                            
                   %Trim(%Char(W_PocRfp)) + ' %Pedidos_PagATiempo ' +                               
                   %Trim(%Char(W_PopPpiF)) + '% a ' +                                               
                   %Trim(%Char(W_PopPpfF)) +                                                        
                   '% PedidosEnPagAntic ' +  %Trim(%Char(W_PocNpe)) +                               
                   ' CupoAsignar $' + %Trim(%Char(W_PocCup)) +                                      
                   ' Valor castigo > ' + %Trim(%Char(W_PocVca));                                    
                                                                                                    
         Write REGRES;                                                                              
                                                                                                    
         TipoTra = 'NuevaPol';                                                                      
         Rango1 = W_PocRip;                                                                         
         Rango2 = W_PocRfp;                                                                         
         Porra1 = W_PopPpiF;                                                                        
         Porra2 = W_PopPpfF;                                                                        
         Rango3 = W_PocNpe;                                                                         
         Cupo =  W_PocCup;                                                                          
         Write REGRES;                                                                              
       EndIf;                                                                                       
                                                                                                    
       //se guarda los registros para luego hacer calculo de pintar                                 
                                                                                                    
       TipoTra = 'CASTIGOSINESTUDIO';                                                               
       CasVlr = W_CasVlr1;                                                                          
       CasPag = W_POCVCA;                                                                           
       Write REGRES;                                                                                
                                                                                                    
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que obtiene la informacion                     //                           
       //------------------------------------------------------------------//                       
     PPrProce          B                                                                            
     DPrProce          Pi                                                                           
      /Free                                                                                         
        //Wsql = 'Select (NROPED-1) ' +                                                             
        Wsql = 'Select (NROPED) ' +                                                                 
               'From ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                              
               ' Where TIPOTRA = ''PagoCredito''';                                                  
                                                                                                    
        Exec Sql                                                                                    
          Prepare C2 From :Wsql;                                                                    
                                                                                                    
        Exec Sql                                                                                    
          Declare C2 Scroll Cursor For C2;                                                          
                                                                                                    
        Exec Sql                                                                                    
          Open C2;                                                                                  
                                                                                                    
        Exec Sql                                                                                    
          fetch C2 Into :W_PedCre;                                                                  
                                                                                                    
        Exec Sql                                                                                    
          Close C2;                                                                                 
                                                                                                    
        Wsql = 'Select  NROPED  ' +                                                                 
               'From ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                              
               ' Where TIPOTRA = ''PagoAnticipado''';                                               
                                                                                                    
        Exec Sql                                                                                    
          Prepare C3 From :Wsql;                                                                    
                                                                                                    
        Exec Sql                                                                                    
          Declare C3 Scroll Cursor For C3;                                                          
                                                                                                    
        Exec Sql                                                                                    
          Open C3;                                                                                  
                                                                                                    
        Exec Sql                                                                                    
          fetch C3 Into :W_PedAnt;                                                                  
                                                                                                    
        Exec Sql                                                                                    
          Close C3;                                                                                 
                                                                                                    
        Wsql = 'Select  PORRA1  ' +                                                                 
               'From ' + %trim(P_Libreria) + '/' +  %trim(P_Archivo) +                              
               ' Where TIPOTRA = ''%CarteraEdades''';                                               
                                                                                                    
        Exec Sql                                                                                    
          Prepare C4 From :Wsql;                                                                    
                                                                                                    
        Exec Sql                                                                                    
          Declare C4 Scroll Cursor For C4;                                                          
                                                                                                    
        Exec Sql                                                                                    
          Open C4;                                                                                  
                                                                                                    
        Exec Sql                                                                                    
          fetch C4 Into :W_PorEda;                                                                  
                                                                                                    
        Exec Sql                                                                                    
          Close C4;                                                                                 
                                                                                                    
                                                                                                    
      /End-Free                                                                                     
     PPrProce          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que obtiene la informacion de las politicas    //                           
       //------------------------------------------------------------------//                       
     PPrPolitic        B                                                                            
     DPrPolitic        Pi                                                                           
      /Free                                                                                         
       PrRango();                                                                                   
       PrvalPar();                                                                                  
        If sqlCode = *Zeros;                                                                        
          If W_POPPPI = *Zeros And W_POPPPF = *Zeros;                                               
            PrAprueba();                                                                            
          Else;                                                                                     
            Wsql = 'SELECT POCNPE, POCCUP ' +                                                       
                     'From Votrea' + %Trim(P_Cia) + '/VTMPOCF ' +                                   
             'WHERE ' +%Trim(%Char(W_PorEda)) + ' between POPPPI and POPPPF '+                      
               'And ' + %Trim(%Char(W_PedCre)) + ' between POCRIP and POCRFP';                      
                                                                                                    
            Exec Sql                                                                                
              Prepare C6 From :Wsql;                                                                
                                                                                                    
            Exec Sql                                                                                
              Declare C6 Scroll Cursor For C6;                                                      
                                                                                                    
            Exec Sql                                                                                
              Open C6;                                                                              
                                                                                                    
            Exec Sql                                                                                
              fetch C6 Into :W_POCNPE, :W_POCCUP;                                                   
                                                                                                    
              If Sqlcode = *Zeros;                                                                  
                PrAprueba();                                                                        
              Else;                                                                                 
                OBSERV = 'Valores no cumple con las políticas de la empresa';                       
              EndIf;                                                                                
                                                                                                    
            Exec Sql                                                                                
              Close C6;                                                                             
          EndIf;                                                                                    
        EndIf;                                                                                      
                                                                                                    
      /End-Free                                                                                     
     PPrPolitic        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que obtiene la informacion de las politicas    //                           
       //------------------------------------------------------------------//                       
     PPrAprueba        B                                                                            
     DPrAprueba        Pi                                                                           
      /Free                                                                                         
       APROBA = 'X';                                                                                
       NEGADO = ' ';                                                                                
       CUPOAS = W_POCCUP;                                                                           
       If W_PedAnt < W_POCNPE;                                                                      
         CONDIC = 'SI';                                                                             
         NPEDCO = (W_POCNPE - W_PedAnt);                                                            
       EndIf;                                                                                       
       W_IndEsc = 'N';                                                                              
      /End-Free                                                                                     
     PPrAprueba        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que obtiene la informacion                     //                           
       //------------------------------------------------------------------//                       
     PPrRango          B                                                                            
     DPrRango          Pi                                                                           
      /Free                                                                                         
       Wsql = 'SELECT POPPPI, POPPPF ' +                                                            
              'From Votrea' + %Trim(P_Cia) + '/VTMPOCF ' +                                          
              'WHERE ' + %Trim(%Char(W_PedCre)) + ' between POCRIP and POCRFP';                     
                                                                                                    
        Exec Sql                                                                                    
          Prepare C9 From :Wsql;                                                                    
                                                                                                    
        Exec Sql                                                                                    
          Declare C9 Scroll Cursor For C9;                                                          
                                                                                                    
        Exec Sql                                                                                    
          Open C9;                                                                                  
                                                                                                    
        Exec Sql                                                                                    
          fetch C9 Into :W_POPPPI, :W_POPPPF;                                                       
                                                                                                    
        // Se agrega validación  09-2018                                                            
        If SqlCod <> 0;                                                                             
          W_POPPPI = 0;                                                                             
          W_POPPPF = 0;                                                                             
        EndIf;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
          Close C9;                                                                                 
                                                                                                    
      /End-Free                                                                                     
     PPrRango          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que consulta el valor del castigo                 //                        
       //------------------------------------------------------------------//                       
     PPrvalCas         B                                                                            
     DPrValCas         Pi                                                                           
      /Free                                                                                         
       Wsql = 'Select CASVLR  ' +                                                                   
               'From ' + %Trim(W_User) + '/REstado ' +                                              
               'Where TipoTra Like (''%Castigo%'')';                                                
                                                                                                    
       Exec Sql                                                                                     
         Prepare C10 From :Wsql;                                                                    
                                                                                                    
       Exec Sql                                                                                     
         Declare C10 Scroll Cursor For C10;                                                         
                                                                                                    
       Exec Sql                                                                                     
         Open C10;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
         fetch C10 Into :W_CasVlr1;                                                                 
                                                                                                    
       Exec Sql                                                                                     
         Close C10;                                                                                 
                                                                                                    
      /End-Free                                                                                     
     PPrValCas         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que consulta el valor de los parametros            //                       
       //------------------------------------------------------------------//                       
     PPrvalPar         B                                                                            
     DPrValPar         Pi                                                                           
      /Free                                                                                         
       Exec Sql                                                                                     
         Close C5;                                                                                  
                                                                                                    
       Wsql = 'SELECT POCNPE, POCCUP, POPPPI, POPPPF, POCRIP, POCRFP, POCVCA ' +                    
                'From Votrea' + %Trim(P_Cia) + '/VTMPOCF ' +                                        
              'WHERE ' + %Trim(%Char(W_PedCre)) + ' between POCRIP and POCRFP ';                    
                                                                                                    
       If W_POPPPI <> *Zeros Or W_POPPPF <> *Zeros;                                                 
         Wsql = %Trim(Wsql) +                                                                       
             ' And ' +%Trim(%Char(W_PorEda)) + ' between POPPPI and POPPPF ';                       
       EndIf;                                                                                       
                                                                                                    
        Exec Sql                                                                                    
          Prepare C5 From :Wsql;                                                                    
                                                                                                    
        Exec Sql                                                                                    
          Declare C5 Scroll Cursor For C5;                                                          
                                                                                                    
        Exec Sql                                                                                    
          Open C5;                                                                                  
                                                                                                    
        Exec Sql                                                                                    
          fetch C5 Into :W_POCNPE, :W_POCCUP, :W_POPPPI, :W_POPPPF, :W_POCRip,                      
                        :W_POCRFP, :W_POCVCA;                                                       
                                                                                                    
      /End-Free                                                                                     
     PPrValPar         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que escribe                                        //                       
       //------------------------------------------------------------------//                       
     PPrContinuar      B                                                                            
     DPrContinuar      Pi                                                                           
      /Free                                                                                         
         PrProce();                                                                                 
         APROBA = ' ';                                                                              
         NEGADO = 'X ';                                                                             
         CONDIC = 'NO';                                                                             
         NPEDCO = 0;                                                                                
         CUPOAS = 0;                                                                                
         OBSERV = ' ';                                                                              
         PrPolitic();                                                                               
         If W_IndEsc = 'N';                                                                         
           TIPOTRA = 'EstudioCredito';                                                              
           Write REGRES;                                                                            
           W_IndEsc = 'S';                                                                          
         EndIf;                                                                                     
      /End-Free                                                                                     
     PPrContinuar      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que busca los demograficos                         //                       
       //------------------------------------------------------------------//                       
     PPrDemografic     B                                                                            
     DPrDemografic     Pi                                                                           
      /Free                                                                                         
       Wsql = 'Select RcdD3c, RcdD7C, RcdD8C, RcdD1E, RcdD8E, RcdD3E, ' +                           
              'RcdD7E ' +                                                                           
              'From Votrea00/Vtmrcaf ' +                                                            
              'Where RcaCia = ' + '''' + %Trim(P_Cia) + '''';                                       
                                                                                                    
       Exec Sql                                                                                     
         Prepare C11 From :Wsql;                                                                    
                                                                                                    
       Exec Sql                                                                                     
         Declare C11 Scroll Cursor For C11;                                                         
                                                                                                    
       Exec Sql                                                                                     
         Open C11;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
         fetch C11 Into :W_RcdD3c, :W_RcdD7C, :W_RcdD8C, :W_RcdD1E,                                 
                        :W_RcdD8E, :W_RcdD3E, :W_RcdD7E;                                            
                                                                                                    
       Exec Sql                                                                                     
         Close C11;                                                                                 
                                                                                                    
       Wsql = 'Select MldM08, MldM03, MldM07, MldM01 ' +                                            
              'From Opf' + %Trim(P_Cia) + '/Lmllst01 ' +                                            
              'Where MlcStÑ = ' + %Trim(%Char(W_Cedula));                                           
                                                                                                    
       Exec Sql                                                                                     
         Prepare C12 From :Wsql;                                                                    
                                                                                                    
       Exec Sql                                                                                     
         Declare C12 Scroll Cursor For C12;                                                         
                                                                                                    
       Exec Sql                                                                                     
         Open C12;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
         fetch C12 Into :W_MldM08, :W_MldM03, :W_MldM07, :W_MldM01;                                 
                                                                                                    
       Exec Sql                                                                                     
         Close C12;                                                                                 
                                                                                                    
      /End-Free                                                                                     
     PPrDemografic     E                                                                            
