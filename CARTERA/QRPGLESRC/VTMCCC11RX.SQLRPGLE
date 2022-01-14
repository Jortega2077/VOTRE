****************************************************************************                        
     **                                                                    *                        
     ** Programa      : VTMCCC11R                                          *                        
     ** Descripciòn:  : Generar Archivo Resumido Partiendo del archivo     *                        
     **                 Estado  RESTADO                                   *                         
     ** Proyecto      : Estados de Cuentas                                 *                        
     ** Autor         : John William Palacio Cardenas PersonalSoft         *                        
     ** Fecha Creaciòn: 09 de Abril de 2015                                *                        
     **                                                                    *                        
     ***********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HDftactgrp(*No)                                                                                
     HTimfmt(*Hms)                                                                                  
     HOption(*Srcstmt:*Nodebugio)                                                                   
                                                                                                    
     **Definición de archivos                                                                       
     FLMLLST01  If   e           K Disk                                                             
     FRESTADO   If A e           K Disk                                                             
     FESTADOL1  If A e           K Disk    UsrOpn                                                   
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DWsql             s           1000a   Inz(*Blanks)                                             
     D$COMA            C                   ''''                                                     
     DUser             S             10A   Inz(*User)                                               
     DW_Nombre         S             30A                                                            
     DW_Zona           S              3A                                                            
     DW_Cedula         S              9  0                                                          
     DW_Contador       S              9  0                                                          
     DW_Contador1      S              9  0                                                          
     DW_Tipo           S             20A                                                            
     DW_val            S              5  0                                                          
     DW_val2           S              5  0                                                          
     DI                S              2  0                                                          
     DT                S              1  0                                                          
     DW_Tot            S              5  0                                                          
     DW_FecFacMax      S              8  0                                                          
     DW_FecVenMax      S              8  0                                                          
     DW_FecFacMaxC     S              8  0                                                          
     DW_FecVenMaxC     S              8  0                                                          
     DW_Campa          S              4A                                                            
     DW_Numdia         S              4  0                                                          
     DW_TotalDia       S              5  0                                                          
     DW_ContReg        S              9  0                                                          
     DW_TotPed         S              5  2                                                          
     DW_IndC           S              1A                                                            
     DW_CasVlr         S             11  2                                                          
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DPgmProcesar      Pr                  Extpgm('VOTREP00/VTMCCC13R')                             
     DXcia                            3                                                             
                                                                                                    
     D                 Ds                                                                           
     DRangoE                          5S 0 Dim(9)                                                   
     DPdR01                    1      5S 0                                                          
     DPdR02                    6     10S 0                                                          
     DPdR03                   11     15S 0                                                          
     DPdR04                   16     20S 0                                                          
     DPdR05                   21     25S 0                                                          
     DPdR06                   26     30S 0                                                          
     DPdR07                   31     35S 0                                                          
     DPdR08                   36     40S 0                                                          
     DPdR09                   41     45S 0                                                          
                                                                                                    
     D                 Ds                                                                           
     DRangoP                          5S 2 Dim(9)                                                   
     DPPR01                    1      5S 2                                                          
     DPPR02                    6     10S 2                                                          
     DPPR03                   11     15S 2                                                          
     DPPR04                   16     20S 2                                                          
     DPPR05                   21     25S 2                                                          
     DPPR06                   26     30S 2                                                          
     DPPR07                   31     35S 2                                                          
     DPPR08                   36     40S 2                                                          
     DPPR09                   41     45S 2                                                          
                                                                                                    
      //estructuras de trabajo                                                                      
     DFields           Ds                                                                           
     DQMlcStÑ                         9  0                                                          
     DQMlnAme                        20                                                             
     DQMlzIpc                         3                                                             
     DQMlcRlm                         9  0                                                          
     DQMlcLas                         3                                                             
     DQMlpRlv                         3                                                             
     DQmlinf1                         4                                                             
                                                                                                    
     DFields2          Ds                                                                           
     DQTipo                          20                                                             
     DQDias                           8                                                             
     DQVl_F                          11  2                                                          
     DQVl_P                          11  2                                                          
     DQSald                          11  2                                                          
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DP_Cia                           3A                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DP_Cia                           3A                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
                                                                                                    
       Open ESTADOL1;                                                                               
                                                                                                    
       Clear Wsql;                                                                                  
       Wsql = 'Select Count(*) From ' + %Trim(User) + '/Estado';                                    
                                                                                                    
       Exec Sql                                                                                     
         Prepare C7 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C7 Scroll Cursor For C7;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C7;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C7 Into :W_ContReg;                                                                  
                                                                                                    
       Exec Sql                                                                                     
         Close C7;                                                                                  
                                                                                                    
       If W_ContReg <> *Zeros;                                                                      
                                                                                                    
         Clear Wsql;                                                                                
         Wsql = 'Select Distinct Cedula From ' + %Trim(User) + '/Estado';                           
                                                                                                    
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
                                                                                                    
         PrCompr();                                                                                 
         PrPagAnt();                                                                                
         PrEdade();                                                                                 
         PrCastigo();                                                                               
         PrProce();                                                                                 
       EndIf;                                                                                       
       Close ESTADOL1;                                                                              
                                                                                                    
       Wsql = 'Select CASVLR  * -1 ' +                                                              
               'From ' + %Trim(User) + '/REstado ' +                                                
               'Where TipoTra Like (''%Castigo%'')';                                                
                                                                                                    
       Exec Sql                                                                                     
         Prepare C8 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C8 Scroll Cursor For C8;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C8;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C8 Into :W_CasVlr;                                                                   
       If SqlCode = *Zeros;                                                                         
         //If W_CasVlr >  20.000;                                                                   
           PgmProcesar(P_Cia);                                                                      
         //Endif;                                                                                   
       ElseIf SqlCode = 100;                                                                        
         PgmProcesar(P_Cia);                                                                        
       EndIf;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close C8;                                                                                  
                                                                                                    
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Graba informacion de la compradora                 //                       
       //------------------------------------------------------------------//                       
     PPrCompr          B                                                                            
     DPrCompr          Pi                                                                           
      /Free                                                                                         
       Clear Wsql;                                                                                  
       Wsql = 'Select MlcStÑ, MlnAme, MlzIpc, MlcRlm, MlcLas, MlpRlv, ' +                           
              ' substr(mlinf1, 7, 4) ' +                                                            
              'From Opf' + %Trim(P_Cia) + '/LMLLST01 ' +                                            
              'Where MlcStÑ = ' + %Trim(%Char(W_Cedula));                                           
                                                                                                    
       Exec Sql                                                                                     
         Prepare C2 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C2 Scroll Cursor For C2;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C2;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C2 Into :Fields;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Close C2;                                                                                  
                                                                                                    
       Clear Wsql;                                                                                  
       Wsql = 'Select Count(*) ' +                                                                  
               'From ' + %Trim(User) + '/Estado ' +                                                 
               'Where Tipo = ''Factura''';                                                          
                                                                                                    
       Exec Sql                                                                                     
         Prepare C3 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C3 Scroll Cursor For C3;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C3;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C3 Into :W_Contador;                                                                 
                                                                                                    
       Exec Sql                                                                                     
         Close C3;                                                                                  
                                                                                                    
       W_Tipo = 'Compradora';                                                                       
                                                                                                    
       Clear REGRES;                                                                                
       Cia = P_Cia;                                                                                 
       Tipotra = W_tipo;                                                                            
       CEDULA = QMLCSTÑ;                                                                            
       NOMBRE =  QMLNAME;                                                                           
       ZONA =  QMLZIPC;                                                                             
       CUPO = QMLCRLM;                                                                              
       TIPCOMP = QMLCLAS;                                                                           
       CLACOMP = QMLPRLV;                                                                           
       Totped  = W_Contador;                                                                        
       Write REGRES;                                                                                
      /End-Free                                                                                     
     PPrCompr          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Graba informacion del rango                    //                           
       //------------------------------------------------------------------//                       
     PPrEdade          B                                                                            
     DPrEdade          Pi                                                                           
      /Free                                                                                         
       For I=1 to 9 By 1;                                                                           
         T = T + 1;                                                                                 
         Clear Wsql;                                                                                
         Wsql = 'Select RcaRi' + %Editc(T:'X') + ', ' +                                             
                ' RCARF' + %Editc(T:'X') +                                                          
                 ' From Votrea00/Vtmrcaf ' +                                                        
                 'Where RcaCia = ' + %Trim(P_Cia);                                                  
                                                                                                    
         Exec Sql                                                                                   
           Prepare C4 From :Wsql;                                                                   
                                                                                                    
         Exec Sql                                                                                   
           Declare C4 Scroll Cursor For C4;                                                         
                                                                                                    
         Exec Sql                                                                                   
           Open C4;                                                                                 
                                                                                                    
         Exec Sql                                                                                   
           fetch C4 Into :W_val, :W_val2;                                                           
                                                                                                    
         Exec Sql                                                                                   
           Close C4;                                                                                
         CLear W_Tot;                                                                               
         If W_val2 <> *Zeros;                                                                       
           Clear Wsql;                                                                              
           Wsql = 'Select Count(*) From Qtemp/Datos ' +                                             
                  ' Where Edades between ' +                                                        
                  %Trim(%Char(W_val)) + ' And '  + %Trim(%Char(W_val2));                            
                                                                                                    
           Exec Sql                                                                                 
             Prepare C5 From :Wsql;                                                                 
                                                                                                    
           Exec Sql                                                                                 
             Declare C5 Scroll Cursor For C5;                                                       
                                                                                                    
           Exec Sql                                                                                 
             Open C5;                                                                               
                                                                                                    
           Exec Sql                                                                                 
             fetch C5 Into :W_Tot;                                                                  
                                                                                                    
           Exec Sql                                                                                 
             Close C5;                                                                              
         EndIf;                                                                                     
         RangoE(i) = W_Tot;                                                                         
       EndFor;                                                                                      
                                                                                                    
       W_Tipo = 'CarteraEdades';                                                                    
                                                                                                    
       Clear REGRES;                                                                                
       Cia = P_Cia;                                                                                 
       Tipotra = W_tipo;                                                                            
       Rango1 = PdR01;                                                                              
       Rango2 = PdR02;                                                                              
       Rango3 = PdR03;                                                                              
       Rango4 = PdR04;                                                                              
       Rango5 = PdR05;                                                                              
       Rango6 = PdR06;                                                                              
       Rango7 = PdR07;                                                                              
       Rango8 = PdR08;                                                                              
       Rango9 = PdR09;                                                                              
       Write REGRES;                                                                                
                                                                                                    
      /End-Free                                                                                     
     PPrEdade          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Graba informacion del pago anticipado             //                        
       //------------------------------------------------------------------//                       
     PPrPagAnt         B                                                                            
     DPrPagAnt         Pi                                                                           
      /Free                                                                                         
       SetLl *Hival Estadol1;                                                                       
       ReadP(N) Estadol1;                                                                           
       Dow Not %Eof(Estadol1);                                                                      
         If Tipo = 'Factura';                                                                       
           If Pag_Antcp = 'PA' And Meda >= Qmlinf1;                                                 
             W_Contador1 = W_Contador1 + 1;                                                         
             If Fecha00001 > W_FecFacMax;                                                           
               W_FecFacMax = FECHA00001;                                                            
             EndIf;                                                                                 
             If Fecha00002 > W_FecVenMax;                                                           
               W_FecVenMax = FECHA00002;                                                            
             EndIf;                                                                                 
           ElseIF  Pag_Antcp = 'CR';                                                                
             If W_Contador1 <> *Zeros;                                                              
               W_Tipo = 'PagoAnticipado';                                                           
               Clear REGRES;                                                                        
               Cia = P_Cia;                                                                         
               Tipotra = W_tipo;                                                                    
               NroPed = W_Contador1;                                                                
               FecFac = %Date(W_FecFacMax);                                                         
               FecVto = %Date(W_FecVenMax);                                                         
               Write REGRES;                                                                        
             EndIf;                                                                                 
             PrPagCre();                                                                            
             Leave;                                                                                 
           EndIf;                                                                                   
         EndIf;                                                                                     
         ReadP(N) Estadol1;                                                                         
       EndDo;                                                                                       
                                                                                                    
       If W_Contador1 <> *Zeros;                                                                    
         WSql = 'Select Count(0) ' +                                                                
                'From Votrea' + %Trim(P_Cia) + '/Vthparf ' +                                        
                'Inner join opcapf/pmllst' + %Trim(P_Cia) +                                         
                ' On MlcStÑ = AdcStÑ ' +                                                            
                'Where AdcStÑ Not In(Select AdcStÑ ' +                                              
                'From Opf' + %Trim(P_Cia) + '/Pardtl ) ' +                                          
                'And AdcStÑ = ' + %Char(QMlcStÑ);                                                   
                                                                                                    
         Exec Sql                                                                                   
           Prepare C9 From :Wsql;                                                                   
                                                                                                    
         Exec Sql                                                                                   
           Declare C9 Scroll Cursor For C9;                                                         
                                                                                                    
         Exec Sql                                                                                   
           Open C9;                                                                                 
                                                                                                    
         Exec Sql                                                                                   
           fetch C9 Into :W_ContReg;                                                                
                                                                                                    
         Exec Sql                                                                                   
           Close C9;                                                                                
                                                                                                    
         If W_ContReg = *Zeros;                                                                     
           W_Tipo = 'PagoAnticipado';                                                               
           Clear REGRES;                                                                            
           Cia = P_Cia;                                                                             
           Tipotra = W_tipo;                                                                        
           If W_ContReg <> *Zeros;                                                                  
             Clear NroPed;                                                                          
           Else;                                                                                    
             NroPed = W_Contador1;                                                                  
           EndIf;                                                                                   
           FecFac = %Date(W_FecFacMax);                                                             
           FecVto = %Date(W_FecVenMax);                                                             
           Write REGRES;                                                                            
         EndIf;                                                                                     
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPrPagAnt         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Graba informacion del pago credito                 //                       
       //------------------------------------------------------------------//                       
     PPrPagCre         B                                                                            
     DPrPagCre         Pi                                                                           
      /Free                                                                                         
       W_IndC = 'N';                                                                                
       Clear Wsql;                                                                                  
       Wsql = 'Create Table Qtemp/Datos (Edades Character ( 8) Not Null With ' +                    
               'Default)';                                                                          
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
       Wsql = 'delete Qtemp/datos';                                                                 
         Exec Sql                                                                                   
         Execute Immediate :Wsql;                                                                   
       W_Contador1 = 1;                                                                             
       W_FecFacMax = FECHA00001;                                                                    
       W_FecVenMax = FECHA00002;                                                                    
       //W_Campa = Meda;                                                                            
       Monitor;                                                                                     
         W_Numdia = %Int(Dias_Mora);                                                                
         If W_Numdia = *Zeros;                                                                      
           Clear W_Campa;                                                                           
         Else;                                                                                      
           W_Campa = Meda;                                                                          
         EndIf;                                                                                     
       On-Error;                                                                                    
         W_Numdia = 0;                                                                              
         Clear W_Campa;                                                                             
       EndMon;                                                                                      
                                                                                                    
       Monitor;                                                                                     
         W_TotalDia = %Int(Dias_Mora);                                                              
       On-Error;                                                                                    
         W_TotalDia = %Int(Dias_Mora);                                                              
       EndMon;                                                                                      
                                                                                                    
       Wsql = 'Insert Into Qtemp/Datos Values(' +                                                   
              $Coma + %Trim(DIAS_MORA) + $Coma + ')';                                               
                                                                                                    
       Exec Sql                                                                                     
         Execute Immediate :Wsql;                                                                   
                                                                                                    
                                                                                                    
       ReadP(N) Estadol1;                                                                           
       Dow Not %Eof(Estadol1);                                                                      
         If Tipo = 'Factura';                                                                       
           If Pag_Antcp = 'CR';                                                                     
             Wsql = 'Insert Into Qtemp/Datos Values(' +                                             
                     $Coma + %Trim(DIAS_MORA) + $Coma + ')';                                        
                                                                                                    
             Exec Sql                                                                               
               Execute Immediate :Wsql;                                                             
                                                                                                    
             W_Contador1 = W_Contador1 + 1;                                                         
             Monitor;                                                                               
               W_TotalDia = W_TotalDia + %Int(Dias_Mora);                                           
             On-Error;                                                                              
               W_TotalDia = W_TotalDia;                                                             
             EndMon;                                                                                
                                                                                                    
             If Fecha00001 > W_FecFacMax;                                                           
               W_FecFacMax = FECHA00001;                                                            
             EndIf;                                                                                 
             If Fecha00002 > W_FecVenMax;                                                           
               W_FecVenMax = FECHA00002;                                                            
             EndIf;                                                                                 
             If W_Campa = *Blanks and W_IndC = 'N';                                                 
               W_IndC = 'S';                                                                        
               Monitor;                                                                             
                 W_Numdia = %Int(Dias_Mora);                                                        
                 If W_Numdia = *Zeros;                                                              
                   Clear W_Campa;                                                                   
                 Else;                                                                              
                   W_Campa = Meda;                                                                  
                 EndIf;                                                                             
               On-Error;                                                                            
                 W_Numdia = 0;                                                                      
                 Clear W_Campa;                                                                     
               EndMon;                                                                              
             EndIf;                                                                                 
                                                                                                    
           ElseIF  Pag_Antcp = 'PA';                                                                
             Leave;                                                                                 
           EndIf;                                                                                   
         EndIf;                                                                                     
         ReadP(N) Estadol1;                                                                         
       EndDo;                                                                                       
                                                                                                    
       W_Tipo = 'PagoCredito';                                                                      
                                                                                                    
       Clear REGRES;                                                                                
       Cia = P_Cia;                                                                                 
       Tipotra = W_tipo;                                                                            
       NroPed = W_Contador1;                                                                        
       FecFac = %Date(W_FecFacMax);                                                                 
       FecVto = %Date(W_FecVenMax);                                                                 
       CamAbo = W_Campa;                                                                            
       NroDia = W_NumDia;                                                                           
       PedPro = W_TotalDia / W_Contador1;                                                           
                                                                                                    
       Write REGRES;                                                                                
       W_TotPed = W_Contador1;                                                                      
                                                                                                    
       Clear W_Contador1;                                                                           
      /End-Free                                                                                     
     PPrPagCre         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Graba informacion de la compradora                 //                       
       //------------------------------------------------------------------//                       
     PPrCastigo        B                                                                            
     DPrCastigo        Pi                                                                           
      /Free                                                                                         
       Clear Wsql;                                                                                  
       Wsql = 'Select Tipo, Dias_Mora, Vr_Factura, Vr_Pago, Saldo ' +                               
               'From ' + %Trim(User) + '/Estado ' +                                                 
               'Where Tipo Like (''%Castigo%'')';                                                   
                                                                                                    
       Exec Sql                                                                                     
         Prepare C6 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C6 Scroll Cursor For C6;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C6;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C6 Into :Fields2;                                                                    
       If Sqlcode = *Zeros;                                                                         
         Clear REGRES;                                                                              
         W_Tipo = QTipo;                                                                            
         CasVlr = QVl_F;                                                                            
         CasPag = QVl_P;                                                                            
         CasSal = QSald;                                                                            
         Cia = P_Cia;                                                                               
         Tipotra = W_tipo;                                                                          
         Monitor;                                                                                   
           NroPed = %Int(%Subst(QDias:1:2));                                                        
         On-error;                                                                                  
           Clear NroPed;                                                                            
         EndMon;                                                                                    
         Write REGRES;                                                                              
       ENdIf;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close C6;                                                                                  
                                                                                                    
      /End-Free                                                                                     
     PPrCastigo        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Graba informacion de los porcentajes           //                           
       //------------------------------------------------------------------//                       
     PPrProce          B                                                                            
     DPrProce          Pi                                                                           
      /Free                                                                                         
         IF W_TotPed =  0;                                                                          
            W_TotPed = 1 ;                                                                          
         ENDIF;                                                                                     
                                                                                                    
       For I=1 to 9 By 1;                                                                           
         RangoP(I) = (RangoE(i) / W_TotPed) * 100;                                                  
       EndFor;                                                                                      
                                                                                                    
       W_Tipo = '%CarteraEdades';                                                                   
                                                                                                    
       Clear REGRES;                                                                                
       Cia = P_Cia;                                                                                 
       Tipotra = W_tipo;                                                                            
       POrra1 = PPR01;                                                                              
       POrra2 = PPR02;                                                                              
       POrra3 = PPR03;                                                                              
       POrra4 = PPR04;                                                                              
       POrra5 = PPR05;                                                                              
       POrra6 = PPR06;                                                                              
       POrra7 = PPR07;                                                                              
       POrra8 = PPR08;                                                                              
       POrra9 = PPR09;                                                                              
       Write REGRES;                                                                                
                                                                                                    
      /End-Free                                                                                     
     PPrProce          E                                                                            
