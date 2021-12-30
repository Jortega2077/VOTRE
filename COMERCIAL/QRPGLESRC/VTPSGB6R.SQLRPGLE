***************************************************************************                         
     ** Programa      : VTPSGB6R                                          *                         
     ** Descripción:  : Pgm Proceso de Generación de datos para Informe de*                         
     **                 Liquidación Manual de Condiciones de Guias        *                         
     ** Proyecto      : Informe Liquidación Manual                        *                         
     ** Autor         : John William Palacio Cardenas - Personalsoft S.A  *                         
     ** Fecha Creación: 06 de Abril de 2016                               *                         
     **********************************************************************                         
                                                                                                    
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definición de Archivo de Trabajo                                                            
     FVTHLCGF   Uf A e           K Disk                                                             
     FLmllst01  IF   E           K Disk                                                             
     FLardtl01  IF   E           K Disk                                                             
                                                                                                    
     ** Definición de Archivo de Pantallas                                                          
                                                                                                    
     ** Definición estructura del sistema                                                           
     D Estado_Pgm     SDs                                                                           
     D   Nom_Pgm         *PROC                                                                      
     D   Msg_ErrID            40     46                                                             
     D   Msg_Err              91    169                                                             
     D   Nom_Job             244    253                                                             
     D   Usuario             254    263                                                             
     D   Nro_Job             264    269                                                             
     DSig_sys                195    196  0                                                          
     DMes_sys                276    277  0                                                          
     DDia_sys                278    279  0                                                          
     DAÑo_sys                280    281  0                                                          
                                                                                                    
     ** Definición de Variables de Trabajo                                                          
     DXpais            s             20    Inz(*Blanks)                                             
     DWsql             s            500a                                                            
     D$COMA            C                   ''''                                                     
     DCmd              s             20                                                             
     DLong             s             15p 5                                                          
     DW_Valor          S             11  2                                                          
     DW_VlpPedido      S             11  2                                                          
     DW_VlpFacturad    S             11  2                                                          
     DW_VlpSuperGan    S             11  2                                                          
     DW_VlpCatalogo    S             11  2                                                          
     DW_VlpInscrip     S             11  2                                                          
     DW_VlpFletes      S             11  2                                                          
     DW_VlpOtros       S             11  2                                                          
     DW_Orden          S              9s 0                                                          
     DW_Contador       S              5  0                                                          
     DW_Efetividad     S              1A                                                            
     DW_Insuficien     S              1A                                                            
     DW_Saldo          S             11  2                                                          
     DW_Meda           S              4A                                                            
     DW_ValTotP        S             15  3                                                          
     DW_GldCte         S              9s 0                                                          
     DPCamE            s              4a   Inz(*Blanks)                                             
     DPCamR            s              4a   Inz(*Blanks)                                             
     DPOper            s              1a   Inz(*Blanks)                                             
     DPnume            s              3a   Inz(*Blanks)                                             
                                                                                                    
     ** Definición de Estructuras de Trabajo                                                        
     DFields           Ds                                                                           
     DW_GldEst                        1A                                                            
     DW_GldCte1                      15s 0                                                          
     DW_GldSct                        1A                                                            
     DW_GldCin                        4A                                                            
     DW_GldCdt                        4A                                                            
                                                                                                    
     ** Definición de Prototipos de Trabajo                                                         
     DPrProcesar       Pr                                                                           
                                                                                                    
     DPgmvalIncent     Pr                  Extpgm('VOTREP00/VTVINC0R')                              
     DXCia                            3                                                             
     DXCed                            9S 0                                                          
     DXCam                            4                                                             
     DXVlpPedido                     11  2                                                          
     DXVlpFacturad                   11  2                                                          
     DXVlpSuperGan                   11  2                                                          
     DXVlpCatalogo                   11  2                                                          
     DXVlpInscrip                    11  2                                                          
     DXVlpFletes                     11  2                                                          
     DXVlpOtros                      11  2                                                          
     DXInsuficiencia                  1                                                             
                                                                                                    
     DComando          Pr                  Extpgm('QCMDEXC')                                        
     D                               20    Options(*Varsize)                                        
     D                                     Const                                                    
     DL                              15p 5                                                          
                                                                                                    
     ** Parametros de Entrada al Programa                                                           
     DMain             Pr                  Extpgm('Main')                                           
     DP_Cia                           3                                                             
     DP_Zon                           3                                                             
     DP_Cam                           4                                                             
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DP_Cia                           3                                                             
     DP_Zon                           3                                                             
     DP_Cam                           4                                                             
                                                                                                    
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPcia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       If P_Cam <> '2009';                                                                          
       PCamE = P_Cam;                                                                               
       PCamR = *Blanks;                                                                             
       POper = '+';                                                                                 
       Pnume = '1';                                                                                 
       ProCampa(P_cia:PCamE:PCamR:POper:Pnume);                                                     
       If PCamR = '2009';                                                                           
        PCamR = '2010';                                                                             
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Delete from VOTREA00/VTHLCGF                                                                
        Where LCGCIA = :P_Cia                                                                       
          And LCGMED = :P_Cam                                                                       
          And LCGZON = :P_Zon;                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Delete from VOTREA00/VTGLCGF                                                                
        Where LCGCIA = :P_Cia                                                                       
          And LCGMED = :P_Cam                                                                       
          And LCGZON = :P_Zon;                                                                      
                                                                                                    
       //Se seleccionan los datos a Procesar                                                        
       Exec SQL                                                                                     
         Declare C1 Cursor for                                                                      
         Select GldEst, GldCte, GldSct, GldCin, GldCdt                                              
         From Votrea00/Vtmgldf                                                                      
         Where GldCia = :P_Cia And GldZon = :P_Zon                                                  
           And GldCin <= :P_Cam                                                                     
           And (GldEst = '' Or (GldEst = 'D' And (GldCdt = :P_Cam Or                                
           GldCdt = :PCamR Or Gldcdt >= :PCamR)));                                                  
                                                                                                    
       Exec Sql                                                                                     
         Open C1;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C1 Into :Fields;                                                                     
                                                                                                    
       //se llevan los registros a la pantalla                                                      
       Dow Sqlcod <> 100;                                                                           
         W_GldCte = W_GldCte1;                                                                      
         LcgCia = P_Cia;                                                                            
         LcgMed = P_Cam;                                                                            
         LcgZon = P_Zon;                                                                            
         PrProcesar();                                                                              
         Exec Sql                                                                                   
           Fetch C1 Into :Fields;                                                                   
       Enddo;                                                                                       
                                                                                                    
       Exec SQL                                                                                     
         close C1;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
       Insert into VOTREA00/VTGLCGF                                                                 
       (Select * From VOTREA00/VTHLCGF                                                              
        Where LCGCIA = :P_Cia                                                                       
          And LCGMED = :P_Cam                                                                       
          And LCGZON = :P_Zon);                                                                     
                                                                                                    
       EndIf;                                                                                       
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que buscar los datos de cada compradora            //                       
       //------------------------------------------------------------------//                       
     PPrProcesar       B                                                                            
     DPrProcesar       Pi                                                                           
      /Free                                                                                         
                                                                                                    
       LcgCod = W_GldCte;                                                                           
       LcgSec = W_GldSct;                                                                           
       LcgOme = W_GldCin;                                                                           
       LcgEst =  W_GldEst;                                                                          
       //If W_GldCdt = P_Cam;                                                                       
       //   LcgEst =  W_GldEst;                                                                     
       //Else;                                                                                      
       //   LcgEst =  W_GldEst;                                                                     
       //EndIf;                                                                                     
       Chain (W_GldCte) Lmllst01;                                                                   
       If %Found(Lmllst01);                                                                         
         LcgNom = MlnAme;                                                                           
         LcgCed = MltFax;                                                                           
         LcgLme = MllMed;                                                                           
       Else;                                                                                        
         Clear LcgNom;                                                                              
         Clear LcgCed;                                                                              
         Clear LcgLme;                                                                              
       EndIf;                                                                                       
                                                                                                    
       //Se Valida si la guia Tiene RUT                                                             
       PrTieneRut();                                                                                
                                                                                                    
       //Se calcula el saldo en cartera y la campaña                                                
       PrSaldo();                                                                                   
                                                                                                    
       //Se invoca el PGM para calcular los valores de los incentivos                               
       Clear W_Insuficien;                                                                          
       PgmvalIncent(P_Cia:W_GldCte:P_Cam:W_VlpPedido:W_VlpFacturad:                                 
                          W_VlpSuperGan:W_VlpCatalogo:W_VlpInscrip:                                 
                          W_VlpFletes:W_VlpOtros:W_Insuficien);                                     
                                                                                                    
       LcgTpe = W_VlpPedido;                                                                        
       LcgVfl = W_VlpFletes;                                                                        
       LcgVin = W_VlpInscrip;                                                                       
       LcgVct = W_VlpCatalogo;                                                                      
       LcgSga = W_VlpSuperGan;                                                                      
       LcgOtr = W_VlpOtros;                                                                         
                                                                                                    
       If W_Insuficien = 'S';                                                                       
         LcgIme = 'X';                                                                              
         LcgLme = P_Cam;                                                                            
       Else;                                                                                        
         Clear LcgIme;                                                                              
       EndIf;                                                                                       
                                                                                                    
       LcgTvp = LcgTpe - (LcgVfl + LcgVin + LcgVct + LcgSga + LcgOtr);                              
                                                                                                    
       //se valida si la Guía realizó un pedido por Web                                             
       PrPedidoWeb();                                                                               
                                                                                                    
       Write(E) RegLcg;                                                                             
       Clear RegLcg;                                                                                
                                                                                                    
      /End-Free                                                                                     
     PPrProcesar       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que indica si la guia tiene RUT                    //                       
       //------------------------------------------------------------------//                       
     PPrTieneRut       B                                                                            
     DPrTieneRut       Pi                                                                           
      /Free                                                                                         
                                                                                                    
       Clear W_Contador;                                                                            
       Exec Sql                                                                                     
         Select Count(1) Into :W_Contador                                                           
         From Votrea00/VTMRTGF                                                                      
         Where RTGCIA = :P_Cia                                                                      
               And RTGIDT = :LcgCed                                                                 
               And RTGEST = '';                                                                     
                                                                                                    
       If W_Contador > *Zeros;                                                                      
         LcgRut = 'X';                                                                              
       Else;                                                                                        
         Clear LcgRut;                                                                              
       EndIf;                                                                                       
                                                                                                    
      /End-Free                                                                                     
     PPrTieneRut       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que indica si la guia realizó un pedido por WEB    //                       
       //------------------------------------------------------------------//                       
     PPrPedidoWeb      B                                                                            
     DPrPedidoWeb      Pi                                                                           
      *Definición de Variables locales                                                              
     DW_Estado         S              1A   Inz(*Blanks)                                             
      /Free                                                                                         
                                                                                                    
       Clear LcgWeb;                                                                                
       Exec Sql                                                                                     
        Select OHRCST Into :W_Estado                                                                
        From Porhdr                                                                                 
        Where OhcStÑ = :W_GldCte                                                                    
          And Ohmeda = :P_Cam                                                                       
          And Ohcmfl = 'N'                                                                          
          And (Ohorty = '005' Or Ohorty = '008');                                                   
                                                                                                    
       If W_Estado <> *Blanks;                                                                      
          If W_Estado = 'S';                                                                        
             LcgWeb = 'X';                                                                          
          Else;                                                                                     
             If W_Insuficien = 'S';                                                                 
                LcgWeb = 'X';                                                                       
             EndIf;                                                                                 
          EndIf;                                                                                    
       EndIf;                                                                                       
                                                                                                    
      /End-Free                                                                                     
     PPrPedidoWeb      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que calcula el saldo en cartera                    //                       
       //------------------------------------------------------------------//                       
     PPrSaldo          B                                                                            
     DPrSaldo          Pi                                                                           
     DSW               s              1  0 Inz(*Zeros)                                              
      /Free                                                                                         
                                                                                                    
       Clear W_Saldo;                                                                               
       Clear W_Orden;                                                                               
       Clear W_Meda;                                                                                
       SetLl (W_GldCte) Lardtl01;                                                                   
       ReadE (W_GldCte) Lardtl01;                                                                   
       DoW Not %Eof(Lardtl01);                                                                      
         W_Saldo += AdtRam;                                                                         
         If AdtRty = 'I' And SW = 0;                                                                
           W_Orden = AdoRdÑ;                                                                        
           W_Meda  = AdmEda;                                                                        
           SW = 1;                                                                                  
         EndIf;                                                                                     
         ReadE (W_GldCte) Lardtl01;                                                                 
       EndDo;                                                                                       
                                                                                                    
       If W_Meda = '';                                                                              
         Exec sql                                                                                   
           Select OhmEda Into :W_Meda                                                               
           From Porhdr                                                                              
           Where OHCSTÑ = :W_GldCte                                                                 
             And OHORDÑ = :W_Orden;                                                                 
       EndIf;                                                                                       
       LcgSdo = W_Saldo;                                                                            
       LcgCsd = W_Meda;                                                                             
                                                                                                    
      /End-Free                                                                                     
     PPrSaldo          E                                                                            
