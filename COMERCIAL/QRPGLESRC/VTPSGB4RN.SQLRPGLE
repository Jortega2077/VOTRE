***************************************************************************                         
     ** Programa      : VTPSGB4R                                          *                         
     ** Descripción:  : Pgm Proceso de Generación de datos                *                         
     ** Proyecto      : Informe Liquidación Manual                        *                         
     ** Autor         : John William Palacio Cardenas - Personalsoft S.A  *                         
     ** Fecha Creación: 06 de Abril de 2016                               *                         
     **********************************************************************                         
                                                                                                    
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definición de Archivo de Trabajo                                                            
     FVTHLMGF   Uf A e           K Disk                                                             
     FVtwccpf   IF   E           K Disk                                                             
     FVtmotof   IF   E           K Disk                                                             
                                                                                                    
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
     DW_Orden          S              9  0                                                          
     DW_Contador       S              5  0                                                          
     DW_Efetividad     S              1A                                                            
     DW_Insuficien     S              1A                                                            
     DW_Cia            S              3A   Inz(*Blanks)                                             
     DwCampAnt         S              4A   Inz(*Blanks)                                             
     DPOper            S              1A   Inz('-')                                                 
     DPnume            S              3A   Inz('1')                                                 
                                                                                                    
     ** Definición de Estructuras de Trabajo                                                        
     DFields           Ds                                                                           
     DW_MlcStÑ                        9  0                                                          
     DW_MlnAme                       30A                                                            
     DW_MltFax                       15  0                                                          
     DW_MldM11                        1A                                                            
     DW_MloMEd                       10A                                                            
     DW_MllMed                       10A                                                            
     DW_MliNf1                        4A                                                            
                                                                                                    
     ** Definición de Prototipos de Trabajo                                                         
     DPrOrden          Pr                                                                           
     DPrInsuFici       Pr                                                                           
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
                                                                                                    
                                                                                                    
     DPgmEfectividad   Pr                  Extpgm('VOTREP00/VTPEFE0R')                              
     DXCia                            3                                                             
     DXCed                            9  0                                                          
     DXOrd                            9  0                                                          
     DXEfe                            1                                                             
                                                                                                    
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPcia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
                                                                                                    
     DPrEfecCamAnt     Pr                  Extpgm('VOTREP00/VTPSGB5R')                              
     DPCia                            3a                                                            
     DPZon                            3a                                                            
     DPCamAnt                         4a                                                            
                                                                                                    
                                                                                                    
     DComando          Pr                  Extpgm('QCMDEXC')                                        
     D                               20    Options(*Varsize)                                        
     D                                     Const                                                    
     DL                              15p 5                                                          
                                                                                                    
     ** Parametros de Entrada al Programa                                                           
     DMain             Pr                  Extpgm('VTPSGB4R')                                       
     DP_Cia                           3                                                             
     DP_Zon                           3                                                             
     DP_Cam                           4                                                             
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DP_Cia                           3                                                             
     DP_Zon                           3                                                             
     DP_Cam                           4                                                             
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       If P_Cam <> '2009';                                                                          
                                                                                                    
       //Se seleccionan los datos a Procesar                                                        
       Exec SQL                                                                                     
         Declare C1 Cursor for                                                                      
         Select MlcStÑ, MlnAme, MltFax, MldM11, MloMed, MllMed,                                     
                Substr(MLINF1, 7, 4)                                                                
         From Lmllst01                                                                              
         Where MldEle = '' And MldM01 = '' And MlzIpc = :P_Zon                                      
           And (MlOmed <= :P_Cam Or Substr(MlOmed, 1, 1) = '9')                                     
           And MlcStÑ = 63335506;                                                                   
                                                                                                    
       Exec Sql                                                                                     
         Open C1;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C1 Into :Fields;                                                                     
                                                                                                    
       //se llevan los registros a la pantalla                                                      
       Dow Sqlcod <> 100;                                                                           
         LmgCia = P_Cia;                                                                            
         LmgMed = P_Cam;                                                                            
         LmgZon = P_Zon;                                                                            
         PrProcesar();                                                                              
         Exec Sql                                                                                   
           Fetch C1 Into :Fields;                                                                   
       Enddo;                                                                                       
                                                                                                    
       Exec SQL                                                                                     
         close C1;                                                                                  
                                                                                                    
        Endif;                                                                                      
        *Inlr = *On;                                                                                
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que buscar los datos de cada compradora            //                       
       //------------------------------------------------------------------//                       
     PPrProcesar       B                                                                            
     DPrProcesar       Pi                                                                           
      /Free                                                                                         
       LmgCod = W_MlcStÑ;                                                                           
       LmgNom = W_MlnAme;                                                                           
       LmgCed = W_MltFax;                                                                           
       LmgSec = W_MldM11;                                                                           
       LmgOme = W_MloMEd;                                                                           
       LmgLme = W_MllMed;                                                                           
       Chain(N) W_MlcStÑ Vtmotof;                                                                   
       If %Found(Vtmotof);                                                                          
         LmgBar = OtoBrr;                                                                           
       Else;                                                                                        
         Clear LmgBar;                                                                              
       EndIf;                                                                                       
                                                                                                    
       Clear LmgCla;                                                                                
       Exec Sql                                                                                     
        Select CcpCla Into :LmgCla                                                                  
        From Votrea00/Vtwccpf                                                                       
        Where CCPCIA = :P_Cia And CCPMED = :P_Cam                                                   
          And CCPCOD = :W_MlcStÑ                                                                    
          And CCPCLA <> ''                                                                          
        FETCH FIRST 1 ROW ONLY;                                                                     
                                                                                                    
       //Chain (P_Cia:P_Zon:P_Cam:W_MlcStÑ) Vtwccpf;                                                
       //If %Found(Vtwccpf);                                                                        
       //  LmgCla = CcpCla;                                                                         
       //Else;                                                                                      
       //  Clear LmgCla;                                                                            
       //EndIf;                                                                                     
                                                                                                    
       If LmgCla = 'I';                                                                             
         LmgNir = 'I';                                                                              
       ElseIf %Subst(LmgCla:1:2) <> 'Ra' and %Subst(LmgCla:1:1) = 'R';                              
         LmgNir = 'R';                                                                              
         Monitor;                                                                                   
           LmgNci = %Int(%Subst(LmgCla:2:9));                                                       
         On-Error;                                                                                  
           Clear LmgNci;                                                                            
         EndMon;                                                                                    
         LmgBme = W_MliNf1;                                                                         
       Else;                                                                                        
         Clear LmgNci;                                                                              
         Clear LmgBme;                                                                              
         Clear LmgNir;                                                                              
       EndIf;                                                                                       
                                                                                                    
       W_Cia = P_Cia;                                                                               
       If P_Cia = '001' And P_Zon = '998';                                                          
          W_Cia = '002';                                                                            
       EndIf;                                                                                       
       Clear W_Insuficien;                                                                          
       PgmvalIncent(W_Cia:W_MlcStÑ:P_Cam:W_VlpPedido:W_VlpFacturad:                                 
                    W_VlpSuperGan:W_VlpCatalogo:W_VlpInscrip:                                       
                    W_VlpFletes:W_VlpOtros:W_Insuficien);                                           
                                                                                                    
       LmgTpe = W_VlpPedido;                                                                        
       LmgTfa = W_VlpFacturad;                                                                      
       LmgVfl = W_VlpFletes;                                                                        
       LmgVin = W_VlpInscrip;                                                                       
       LmgVct = W_VlpCatalogo;                                                                      
       LmgSga = W_VlpSuperGan;                                                                      
       LmgOtr = W_VlpOtros;                                                                         
       If %Trim(W_Insuficien) = 'S';                                                                
         LmgIme = 'X';                                                                              
         LmgPef = 'X';                                                                              
         LmgLme = P_Cam;                                                                            
       Else;                                                                                        
         Clear LmgIme;                                                                              
         Clear LmgPef;                                                                              
       EndIf;                                                                                       
                                                                                                    
       LmgTvp = LmgTpe - (LmgVfl + LmgVin + LmgVct + LmgSga + LmgOtr);                              
                                                                                                    
       //If LmgIme = *Blanks;                                                                       
       //  PrOrden();                                                                               
       //  If W_Orden <> 0;                                                                         
       //     PgmEfectividad(P_Cia:W_MlcStÑ:W_Orden:W_Efetividad);                                  
       //     If W_Efetividad = 'S';                                                                
       //       LmgPef = 'X';                                                                       
       //     EndIf;                                                                                
       //  EndIf;                                                                                   
       //EndIf;                                                                                     
                                                                                                    
       Write(E) RegLmg;                                                                             
       Clear RegLmg;                                                                                
      /End-Free                                                                                     
     PPrProcesar       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que buscar la insuficiencia de mercancia           //                       
       //------------------------------------------------------------------//                       
     PPrInsuFici       B                                                                            
     DPrInsuFici       Pi                                                                           
      /Free                                                                                         
       Clear W_Contador;                                                                            
       Exec Sql                                                                                     
         Select Count(1) Into :W_Contador                                                           
         From Porhdr Inner Join Lordtl01 On OhordÑ = OdordÑ                                         
         Where OhcstÑ = :W_MlcStÑ And                                                               
               Ohmeda = :P_Cam And                                                                  
               Ohrcst = 'C' And                                                                     
               Odlccd In ('INS', 'BAC', 'CCD');                                                     
                                                                                                    
      /End-Free                                                                                     
     PPrInsuFici       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que buscar la orden                                //                       
       //------------------------------------------------------------------//                       
     PPrOrden          B                                                                            
     DPrOrden          Pi                                                                           
      /Free                                                                                         
       Clear W_Orden;                                                                               
       Exec Sql                                                                                     
         Select AdoRdÑ Into :W_Orden                                                                
         From Pardtl                                                                                
         Where AdcStÑ = :W_MlcStÑ And AdTRTY = 'I' And                                              
               SubStr(ADDEPT, 1, 4) = :P_Cam                                                        
               Order by Rrn(Pardtl) Desc                                                            
               Fetch first 1 row only;                                                              
                                                                                                    
      /End-Free                                                                                     
     PPrOrden          E                                                                            
