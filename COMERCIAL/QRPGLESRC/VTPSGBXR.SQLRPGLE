      **********************************************************************                        
      ** Programa      : VTPSGBXR                                         **                        
      ** Descripciòn:  : PGM que Llena el Archivo Vtwlagf                 **                        
      ** Proyecto      : Plan Guias                                       **                        
      ** Autor         : (Jortega)                                        **                        
      ** Fecha Creaciòn: 25 de Julio de 2016                              **                        
      **********************************************************************                        
     ** Definiciòn Directivas de Compilación                                                        
     HDftactgrp(*No)                                                                                
     HOption(*NoDebugIo)                                                                            
                                                                                                    
      *Definición de Archivos de Trabajo                                                            
     FVtwlagf   If a e           K Disk                                                             
     FVtwdigf   If a e           K Disk                                                             
                                                                                                    
      *Definición de Variables de Trabajo                                                           
     DSqlStr           s           2000a   Inz(*Blanks)                                             
     DWcampaX          s              4a   Inz(*Blanks)                                             
     DWcampaY          s              4a   Inz(*Blanks)                                             
     DPCamE            s              4a   Inz(*Blanks)                                             
     DPCamR            s              4a   Inz(*Blanks)                                             
     DPOper            s              1a   Inz(*Blanks)                                             
     DPnume            s              3a   Inz(*Blanks)                                             
     DIcaso            s              7a   Inz(*Blanks)                                             
     DIfecha           s             10a   Inz(*Blanks)                                             
     DIcedula          s             30a   Inz(*Blanks)                                             
     DOrespuesta       s              1a   Inz(*Blanks)                                             
     DWCedreempla      s              9  0  Inz(*Zeros)                                             
     DUser             s             10    Inz(*User)                                               
     DWOhmeda          s             10a   Inz(*Blanks)                                             
     DWohordÑ          s              9  0 Inz(*Zeros)                                              
     DWOhrcst          s              1a   Inz(*Blanks)                                             
     DWOhcstÑ          s              9  0 Inz(*Zeros)                                              
     DW_Orden          s              9  0 Inz(*Zeros)                                              
     DXreg             s             30a   Inz(*Blanks)                                             
     DOValor           s             11  2 Inz(*Zeros)                                              
     DOorden           s              9  0 Inz(*Zeros)                                              
     DW_Efetividad     s              1a   Inz(*Blanks)                                             
     DQnro             s              4  0 Inz(*Zeros)                                              
                                                                                                    
     ** Definicion de Estructuras de Trabajo a Utilizar                                             
     DFilesFile        Ds                                                                           
     DWgldcte                         9  0 Inz(*Zeros)                                              
     DWlmgcia                         3a   Inz(*Blanks)                                             
     DWlmgmed                         4a   Inz(*Blanks)                                             
     DWlmgzon                         3a   Inz(*Blanks)                                             
     DWlmgsec                         1a   Inz(*Blanks)                                             
     DWlmgced                        15s 0 Inz(*Zeros)                                              
     DWlmgcod                         9s 0 Inz(*Zeros)                                              
     DWlmgtfa                        11s 2 Inz(*Zeros)                                              
     DWlmgnir                        10a   Inz(*Blanks)                                             
     DWlmgbme                         4a   Inz(*Blanks)                                             
     DWlmgnci                         3s 0 Inz(*Zeros)                                              
     DWlmgcla                        10a   Inz(*Blanks)                                             
     DWlmgtvp                        11s 2 Inz(*Zeros)                                              
     DWlmgome                         4a   Inz(*Blanks)                                             
     DWlmgtpe                        11s 2 Inz(*Zeros)                                              
     DWgldcdt                         4a   Inz(*Blanks)                                             
                                                                                                    
     DPrVtwlagf        Pr                                                                           
     DPrVtwlagfB       Pr                                                                           
     DPrVtwdigf        Pr                                                                           
     DPrBusOrden       Pr                                                                           
     DPrInsuficienciaCte...                                                                         
     D                 Pr             9  0                                                          
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DP_Cia                           3a                                                            
     DP_Cam                           4a                                                            
     DP_Zon                           3a                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DP_Cia                           3a                                                            
     DP_Cam                           4a                                                            
     DP_Zon                           3a                                                            
                                                                                                    
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPCia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
                                                                                                    
     DPgmNewPlanG      Pr                  Extpgm('VOTREP00/VTMPGB0R')                              
     DPcia                            3a                                                            
     DIzona                           3a                                                            
     DIcaso                           7a                                                            
     DIcedula                        30a                                                            
     DIfecha                         10a                                                            
     DOrespuesta                      1a                                                            
                                                                                                    
     ** Consulta efectividad de Pedido                                                              
     DPgmEfectividad   Pr                  Extpgm('VOTREP00/VTPEFE0R')                              
     DXCia                            3                                                             
     DXCed                            9  0                                                          
     DXOrd                            9  0                                                          
     DXEfe                            1A                                                            
                                                                                                    
     DPgmInformesR     Pr                  Extpgm('VOTREP00/VTUPEI8R')                              
     DPciaW                           3                                                             
     DPcamW                           4                                                             
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /Free                                                                                         
       WcampaY = P_Cam;                                                                             
       WcampaX = P_Cam;                                                                             
       //Invocar programa para calculo de campaña anterior                                          
       PCamE=WcampaX;                                                                               
       PCamR=*Blanks;                                                                               
       POper='-';                                                                                   
       Pnume='001';                                                                                 
       If WcampaX = '2010';                                                                         
        Pnume = '002';                                                                              
       Endif;                                                                                       
       ProCampa(P_cia:PCamE:PCamR:POper:Pnume);                                                     
       WcampaX = PCamR;                                                                             
                                                                                                    
        Exec SQL                                                                                    
         Declare C1 Cursor For                                                                      
         Select Distinct                                                                            
         Gldcte, Lmgcia, Lmgmed, Lmgzon, Lmgsec, Lmgced, Lmgcod, Lmgtfa,                            
         Lmgnir, Lmgbme, Lmgnci, Lmgcla, Lmgtvp, Lmgome, Lmgtpe, Gldcdt                             
         From Votrea00/Vtglmgf Inner Join Votrea00/Vtmgldf On                                       
              Lmgcia = Gldcia And                                                                   
              Lmgzon = Gldzon And                                                                   
              Lmgsec = Gldsct                                                                       
         Where Lmgcia = :P_cia And                                                                  
               Lmgmed = :P_cam And                                                                  
               Lmgzon = :P_zon And                                                                  
               (Gldest = ' ' Or (Gldest = 'D' And (Gldcdt = :WcampaX                                
               Or Gldcdt = :WcampaY Or Gldcdt >= :WcampaX))) And                                    
               Lmgsec <> ' '                                                                        
         Order By Lmgsec Asc;                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Open C1;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C1 Into :FilesFile;                                                                   
                                                                                                    
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
        Icaso = *Blanks;                                                                            
        WCedreempla = *Zeros;                                                                       
                                                                                                    
        Exec Sql                                                                                    
         Select Rsgcgr Into :WCedreempla                                                            
         From Votrea00/Vtmrsgf                                                                      
         Where Rsgcia = :P_cia And (Rsgtpo = ' ' Or Rsgtpo = 'P') And                               
               Rsgcga = :WGldcte And Rsgzng = :P_zon And Rsgscg = :Wlmgsec                          
               And :P_cam BetWeen Rsgcpi And Rsgcpf                                                 
         Order By Rsgcpi Asc                                                                        
         Fetch First 1 Row Only;                                                                    
                                                                                                    
        If WCedreempla > *Zeros;                                                                    
         WGldcte = WCedreempla;                                                                     
        Endif;                                                                                      
                                                                                                    
        Icedula = %Trim(%Editc(Wgldcte:'Z'));                                                       
        Ifecha = *Blanks;                                                                           
        Orespuesta = *Blanks;                                                                       
        PgmNewPlanG(P_Cia:P_Zon:Icaso:Icedula:Ifecha:Orespuesta);                                   
        If Orespuesta = 'S';                                                                        
         PrVtwlagf();                                                                               
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch C1 Into :FilesFile;                                                                  
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close C1;                                                                                   
                                                                                                    
       // Marca las Compradoras Dadas de Baja                                                       
       If Orespuesta = 'S';                                                                         
        PrVtwlagfB();                                                                               
        PgmInformesR(P_Cia:P_Cam);                                                                  
       Endif;                                                                                       
       *InLr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento que Graba Registros en el Archivo Vtwlagf          /                         
      //------------------------------------------------------------------/                         
     PPrVtwlagf        B                                                                            
     DPrVtwlagf        Pi                                                                           
      *Definición de Variables de Trabajo                                                           
     DIcaso            s              7a   Inz(*Blanks)                                             
     DIdx              s              2  0 Inz(*Zeros)                                              
     DNro              s              1  0 Inz(1)                                                   
     DOnro             s              9  0 Inz(0)                                                   
      /Free                                                                                         
       Idx = 1;                                                                                     
       If Wlmgtpe <> *Zeros;                                                                        
        Nro = 2;                                                                                    
       Endif;                                                                                       
       If Wlmgnir <> *Blanks;                                                                       
        Nro = 3;                                                                                    
       Endif;                                                                                       
       If Orespuesta = 'S';                                                                         
        Exec Sql                                                                                    
         Delete From Votrea00/Vtwlagf                                                               
         Where Lagcia = :P_Cia And                                                                  
               Lagcdg = :Wgldcte And                                                                
               Lagcmp = :P_cam And                                                                  
               Lagcco = :WLmgcia And                                                                
               Lagcto = :Wlmgcod And                                                                
               Lagnrc <> ' ';                                                                       
       Endif;                                                                                       
                                                                                                    
       For Idx = 1 To Nro By 1;                                                                     
        If P_Cam <= Wgldcdt Or Wgldcdt = *Blanks;                                                   
        WOhmeda = *Blanks;                                                                          
        WohordÑ = *Zeros;                                                                           
        WOhrcst = *Blanks;                                                                          
        WOhcstÑ = *Zeros;                                                                           
        W_Orden = *Zeros;                                                                           
        Icaso = *Blanks;                                                                            
        Onro = *Zeros;                                                                              
        Exec Sql                                                                                    
         Select Count(0) Into :Onro                                                                 
         From Votrea00/Vtwlagf                                                                      
         Where Lagcmp = :P_cam And                                                                  
               Lagcco = :P_cia And                                                                  
               Lagcto = :Wlmgcod And                                                                
               Lagtpv In ('A', 'E');                                                                
                                                                                                    
        If Onro <= *Zeros;                                                                          
        Chain (P_Cia:Wgldcte:P_cam:Wlmgzon:WLmgcia:Wlmgcod:Icaso) Vtwlagf;                          
        If Not %Found(Vtwlagf);                                                                     
         Lagcia = P_Cia;                                                                            
         Lagcdg = Wgldcte;                                                                          
         Lagcmp = P_cam;                                                                            
         Lagzna = Wlmgzon;                                                                          
         Lagcco = WLmgcia;                                                                          
         Lagcto = Wlmgcod;                                                                          
         Lagnrc = Icaso;                                                                            
         Lagest = *Zeros;                                                                           
         Lagesq = *Zeros;                                                                           
         Lagsub = *Zeros;                                                                           
         If Idx = 1;                                                                                
          Lagtpv = 'A';                                                                             
          Lagvlp = *Zeros;                                                                          
          Lagnor = *Zeros;                                                                          
         Endif;                                                                                     
         If Idx = 2;                                                                                
          PrBusOrden();                                                                             
          Lagtpv = 'P';                                                                             
         Endif;                                                                                     
         If Idx = 3;                                                                                
          Lagtpv = 'I';                                                                             
          Lagvlp = *Zeros;                                                                          
          Lagnor = *Zeros;                                                                          
         Endif;                                                                                     
         Lagusr = User;                                                                             
         Lagfec = %Date();                                                                          
         Laghor = %Time();                                                                          
         Write Reglag;                                                                              
         PrVtwdigf();                                                                               
        Endif;                                                                                      
        Endif;                                                                                      
        Endif;                                                                                      
       EndFor;                                                                                      
      /End-Free                                                                                     
     PPrVtwlagf        E                                                                            
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento Selección Registros ha Procesar                    /                         
      //------------------------------------------------------------------/                         
     PPrBusOrden       B                                                                            
     DPrBusOrden       Pi                                                                           
      *Definición de Variables de Trabajo                                                           
     D$coma            c                   ''''                                                     
      /Free                                                                                         
       W_Efetividad = *Blanks;                                                                      
       Ovalor = *Zeros;                                                                             
       Oorden = *Zeros;                                                                             
       Qnro = *Zeros;                                                                               
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Ohmeda, OhordÑ, Ohrcst, OhcstÑ ' +                                          
                'From Opf' + %Trim(P_Cia) + '/Porhdr Inner Join Opf' +                              
                %Trim(P_Cia) +'/Lmllst01 On OhcstÑ=MlcstÑ And ' +                                   
                'Ohmeda = ' + $Coma + %Trim(P_cam) + $Coma + ' ' +                                  
                'Where OhcstÑ = ' + %Trim(%Char(Wgldcte)) + ' And ' +                               
                'Ohcmfl= ''N'' ' +                                                                  
                'Order By Ohmeda Asc';                                                              
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur2 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor2 Cursor For Cur2;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor2;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor2 Into :WOhmeda, :WOhordÑ, :WOhrcst, :WOhcstÑ;                                  
                                                                                                    
       Dow Sqlcod <> 100 And SqlCod >= *Zeros;                                                      
        If WOhrcst = 'S';                                                                           
         PgmEfectividad(P_Cia:WOhcstÑ:W_Orden:W_Efetividad);                                        
         If W_Efetividad = 'S';                                                                     
          Ovalor = Wlmgtpe;                                                                         
          Oorden = W_Orden;                                                                         
         EndIF;                                                                                     
        EndIF;                                                                                      
         If WOhrcst = 'C';                                                                          
          Qnro = PrInsuficienciaCte();                                                              
          If Qnro > 0;                                                                              
           Ovalor = Wlmgtpe;                                                                        
           Oorden = W_Orden;                                                                        
          EndIf;                                                                                    
         EndIF;                                                                                     
        Exec Sql                                                                                    
         Fetch Cursor2 Into :WOhmeda, :WOhordÑ, :WOhrcst, :WOhcstÑ;                                 
        EndDo;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor2;                                                                             
                                                                                                    
      /End-Free                                                                                     
     PPrBusOrden       E                                                                            
                                                                                                    
      //--------------------------------------------------------------------//                      
      // Procedimiento Valida si el Pedido fue Clancelado por Insuficiencia //                      
      // de Mercancía                                                       //                      
      //--------------------------------------------------------------------//                      
     PPrInsuficienciaCte...                                                                         
     P                 B                                                                            
     DPrInsuficienciaCte...                                                                         
     D                 Pi             9  0                                                          
      ** Definicion de Variables a Utilizar                                                         
     DQnro             s              9  0 Inz(*Zeros)                                              
      /Free                                                                                         
        SqlStr = *Blanks;                                                                           
        SqlStr = 'Select Count(Distinct(OdordÑ)) ' +                                                
                 'From Opf' + P_Cia + '/Pordtl ' +                                                  
                 'Where Odscus = ' + %Trim(%Editc(WLmgcod:'Z')) +                                   
                 ' And OdordÑ = ' + %Trim(%Editc(WOhordÑ:'Z')) +                                    
             ' And (Odlccd = ''INS''' + ' Or Odlccd = ''BAC''' + ' Or ' +                           
             'Odlccd = ''CCD''' + ')';                                                              
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur1q From :SqlStr;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1q Cursor For Cur1q;                                                          
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1q;                                                                              
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1q Into :Qnro;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor1q;                                                                             
                                                                                                    
       Return Qnro;                                                                                 
      /End-Free                                                                                     
     PPrInsuficienciaCte...                                                                         
     P                 E                                                                            
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento que Graba Registros en el Archivo Vtwlagf          /                         
      //------------------------------------------------------------------/                         
     PPrVtwlagfB       B                                                                            
     DPrVtwlagfB       PR                                                                           
      *Definición de Variables de Trabajo                                                           
     DGldciaW          s              3a   Inz(*Blanks)                                             
     DGldcteW          s              9  0 Inz(*Zeros)                                              
     DGldzonW          s              3a   Inz(*Blanks)                                             
     DGldsctW          s              3a   Inz(*Blanks)                                             
     DMlcstÑW          s              9  0 Inz(*Zeros)                                              
     DCampaBa          s              4a   Inz(*Blanks)                                             
     DOnro             s              9  0 Inz(0)                                                   
                                                                                                    
      /Free                                                                                         
        Exec SQL                                                                                    
         Declare C2 Cursor For                                                                      
         Select Gldcia, Gldcte, Gldzon, Gldsct, MlcstÑ,                                             
                Substr(Mlinf1, 7, 4)                                                                
         From Votrea00/Vtmgldf Inner Join Lmllst01 On Mldm11 = Gldsct                               
              And Trim(Mlzipc) = Gldzon                                                             
         Where Gldcia = :P_cia And                                                                  
               Gldzon = :P_zon And                                                                  
               (Gldest = ' ' Or (Gldest = 'D' And (Gldcdt = :WcampaX                                
               Or Gldcdt >= :WcampaX                                                                
               Or Gldcdt = :WcampaY))) And (Mldele = 'D' Or Mldm01 <> ' ')                          
               And (Substr(Mlinf1, 7, 4) = :PCamE Or                                                
               Substr(Mlinf1, 7, 4) = :PCamR)                                                       
               And GldSct <> ' ';                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Open C2;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C2 Into :GldciaW, :GldcteW, :GldzonW, :GldsctW, :MlcstÑW,                             
                      :CampaBa;                                                                     
                                                                                                    
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
        If CampaBa <> *Blanks;                                                                      
         Icedula = %Trim(%Editc(GldcteW:'Z'));                                                      
         Ifecha = *Blanks;                                                                          
         Orespuesta = *Blanks;                                                                      
         PgmNewPlanG(GldciaW:GldzonW:Icaso:Icedula:Ifecha:Orespuesta);                              
         If Orespuesta = 'S';                                                                       
          WOhmeda = *Blanks;                                                                        
          WohordÑ = *Zeros;                                                                         
          WOhrcst = *Blanks;                                                                        
          WOhcstÑ = *Zeros;                                                                         
          W_Orden = *Zeros;                                                                         
          Icaso = *Blanks;                                                                          
        Onro = *Zeros;                                                                              
        Exec Sql                                                                                    
         Select Count(0) Into :Onro                                                                 
         From Votrea00/Vtwlagf                                                                      
         Where Lagcmp = :CampaBa And                                                                
               Lagcco = :GldciaW And                                                                
               Lagcto = :MlcstÑW And                                                                
               Lagtpv In ('A', 'E');                                                                
                                                                                                    
        If Onro <= *Zeros;                                                                          
          Chain (GldciaW:GldcteW:CampaBa:GldzonW:GldciaW:MlcstÑW:Icaso) Vtwlagf;                    
          If Not %Found(Vtwlagf);                                                                   
           Lagcia = GldciaW;                                                                        
           Lagcdg = GldcteW;                                                                        
           Lagcmp = CampaBa;                                                                        
           Lagzna = GldzonW;                                                                        
           Lagcco = GldciaW;                                                                        
           Lagcto = MlcstÑW;                                                                        
           Lagnrc = Icaso;                                                                          
           Lagest = *Zeros;                                                                         
           Lagesq = *Zeros;                                                                         
           Lagsub = *Zeros;                                                                         
           Lagtpv = 'B';                                                                            
           Lagvlp = *Zeros;                                                                         
           Lagnor = *Zeros;                                                                         
           Lagusr = User;                                                                           
           Lagfec = %Date();                                                                        
           Laghor = %Time();                                                                        
           Write Reglag;                                                                            
           PrVtwdigf();                                                                             
          Endif;                                                                                    
         Endif;                                                                                     
        Endif;                                                                                      
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch C2 Into :GldciaW, :GldcteW, :GldzonW, :GldsctW, :MlcstÑW,                            
                       :CampaBa;                                                                    
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close C2;                                                                                   
                                                                                                    
      /End-Free                                                                                     
     PPrVtwlagfB       E                                                                            
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento que Graba Registros en el Archivo Vtwdigf          /                         
      //------------------------------------------------------------------/                         
     PPrVtwdigf        B                                                                            
     DPrVtwdigf        PR                                                                           
      *Definición de Variables de Trabajo                                                           
                                                                                                    
      /Free                                                                                         
       Chain (Icaso:Lagcco:Lagcto:Lagcia:Lagcdg) Vtwdigf;                                           
       If Not %Found(Vtwdigf);                                                                      
        Dignrc = Lagnrc;                                                                            
        Digcco = Lagcco;                                                                            
        Digcto = Lagcto;                                                                            
        Digcce = Lagcia;                                                                            
        Digcte = Lagcdg;                                                                            
        Digcmp = Lagcmp;                                                                            
        Digstn = *Blanks;                                                                           
        Digins = *Blanks;                                                                           
        Digbaj = *Blanks;                                                                           
        If Lagtpv = 'A';                                                                            
         Digstn = Lagtpv;                                                                           
        Endif;                                                                                      
        If Lagtpv = 'I';                                                                            
         Digins = Lagtpv;                                                                           
        Endif;                                                                                      
        If Lagtpv = 'B';                                                                            
         Digbaj = Lagtpv;                                                                           
        Endif;                                                                                      
        Digzon = Lagzna;                                                                            
        Write Regdig;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrVtwdigf        E                                                                            
