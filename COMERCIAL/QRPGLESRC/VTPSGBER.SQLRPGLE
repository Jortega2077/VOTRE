     *****************************************************************************                  
      ** Programa     : VTPSGBER                                                **                  
      ** Descripciòn: : Pgm que Valida Nro de Inscripciones por Campaña         **                  
      ** Proyecto     : Plan Guias                                              **                  
      ** Autor        : Edgar Ivan Maldonado Perez - PERSONALSOFT               **                  
      ** Fecha Creaciòn: 25 de Mayo de 2016                                     **                  
      ****************************************************************************                  
     HDftactgrp(*No)                                                                                
     HOption(*NoDebugIo)                                                                            
                                                                                                    
     ** Definiciòn de Archivo de Trabajo                                                            
     ** maestro parametros inactivas guias por rut                                                  
     FVtwidff   Uf a e           k Disk                                                             
     FVttdngf   Uf a e           k Disk                                                             
     FVtwpcgf   Uf a e           k Disk                                                             
     FVtglmgf   If   e           k Disk                                                             
                                                                                                    
      *Definición de variables de trabajo                                                           
     DOcampa           s              4a    Inz(*Blanks)                                            
     DWcod             s              4  0 Inz(5)                                                   
     DWoval            s             11  2                                                          
     DWcte             s              9  0 Inz(*Zeros)                                              
     DWzon             s              3a   Inz(*Blanks)                                             
     DWtrr             s              1a   Inz(*Blanks)                                             
     DWnro             s              4  0 Inz(*Zeros)                                              
     DYnro             s              4  0 Inz(*Zeros)                                              
     DWsec             s              3a   Inz(*Blanks)                                             
     Dknro             s              9s 0 Inz(*zeros)                                              
     DOrespuesta       s              1a   Inz(*Blanks)                                             
     Dwregio           s            100a   Inz(*Blanks)                                             
     DXregio           s             10a   Inz(*Blanks)                                             
     dWplan            s             10a   Inz(*Blanks)                                             
     DUser             s             10a   Inz(*User)                                               
     DWced             s             10a   Inz(*Blanks)                                             
     DWdesc            s            200a   Inz(*Blanks)                                             
     DWcampaX          s              4a   Inz(*Blanks)                                             
     DWcampaY          s              4a   Inz(*Blanks)                                             
     DWcmpIni          s              4a   Inz(*Blanks)                                             
     DPCamE            s              4a   Inz(*Blanks)                                             
     DPCamR            s              4a   Inz(*Blanks)                                             
     DPOper            s              1a   Inz(*Blanks)                                             
     DPnume            s              3a   Inz(*Blanks)                                             
     DIcaso            s              7a   Inz(*Blanks)                                             
     DIfecha           s             10a   Inz(*Blanks)                                             
     DIcedula          s             30a   Inz(*Blanks)                                             
     DOrespuestaP      s              1a   Inz(*Blanks)                                             
     DWCedreempla      s              9  0  Inz(*Zeros)                                             
     DWCedAux          s              9  0  Inz(*Zeros)                                             
     DWDngvlr          s             11  2 Inz(*Zeros)                                              
     DXValor           s             11  2 Inz(*Zeros)                                              
     Dw_IndNA          s               n   Inz(*Off)                                                
                                                                                                    
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
                                                                                                    
      *Definición de Procedimientos                                                                 
      * programa LLAMADO VARIAS VECES                                                               
     DPgmNumero        Pr                  Extpgm('VOTREP00/VTMPGB9R')                              
     DXcia                            3                                                             
     DXreg                           30                                                             
     DXzon                            3                                                             
     DXsec                            3                                                             
     DXcam                            4                                                             
     DXoval                           4  0                                                          
                                                                                                    
      *Programa que trae la region                                                                  
     DPgmregio         Pr                  Extpgm('VOTREP00/VTMGOIGR')                              
     DXcia                            3                                                             
     DXzon                            3                                                             
     DXoreg                          10                                                             
     DXced                           10                                                             
     DXplan                          10                                                             
                                                                                                    
                                                                                                    
      *Definición de Procedimientos                                                                 
     DPrResumen        Pr                                                                           
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
     DOrespuestaP                     1a                                                            
                                                                                                    
     DPrInsGuiasNueva  Pr              n                                                            
      //////////////////////////////////////////////////////////////////////                        
      //                                                                  //                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //                                                                  //                        
      //////////////////////////////////////////////////////////////////////                        
      *Definición de variables de trabajo                                                           
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
       proCampa(P_Cia:PCamE:PCamR:POper:Pnume);                                                     
       P_Cam = PCamR;                                                                               
       WcampaX = P_Cam;                                                                             
                                                                                                    
        Exec SQL                                                                                    
        Declare cursor1 Cursor For                                                                  
         Select Gldcte, Gldzon, Gldsct, Gldtrr, Gldcin                                              
          From Votrea00/Vtmgldf                                                                     
          Where Gldcia = :P_cia and                                                                 
                Gldzon = :P_zon and                                                                 
                ((Gldest = ' ' And Gldcin <= :WcampaX) Or                                           
            (Gldest = 'D' And Gldcin <= :PCamR And (Gldcdt = :WcampaX                               
            Or Gldcdt = :WcampaY Or Gldcdt >= :WcampaX)))                                           
            And Gldsct <> ' ';                                                                      
                                                                                                    
        Exec Sql                                                                                    
          Open cursor1;                                                                             
                                                                                                    
        Exec Sql                                                                                    
          Fetch Cursor1 Into :Wcte, :Wzon,:Wsec,:Wtrr,:WcmpIni;                                     
                                                                                                    
        Dow sqlcod=*Zeros;                                                                          
          Icaso = *Blanks;                                                                          
          Icedula = %Trim(%Editc(Wcte:'Z'));                                                        
          Ifecha = *Blanks;                                                                         
          OrespuestaP = *Blanks;                                                                    
          PgmNewPlanG(P_Cia:P_Zon:Icaso:Icedula:Ifecha:OrespuestaP);                                
          If OrespuestaP = 'S';                                                                     
         //programa para recuperar la region                                                        
         CallP(e) Pgmregio(P_cia:Wzon:Xregio:wced:wplan);                                           
                                                                                                    
         // porgrama para N                                                                         
         Wregio = Xregio;                                                                           
         Callp(e) PgmNumero(p_cia:Wregio:Wzon:Wsec:P_cam:Ynro);                                     
                                                                                                    
        Clear w_IndNA;                                                                              
        If PrInsGuiasNueva() = *Off Or w_IndNA = *Off;                                              
        Exec SQL                                                                                    
           Select count(0) into :Wnro                                                               
            From Votrea00/Vtglmgf Inner Join Votrea00/Vtmgldf On                                    
                  Lmgcia = Gldcia And                                                               
                  Lmgzon = Gldzon And                                                               
                  Lmgsec = Gldsct                                                                   
           Where Lmgcia = :P_cia And                                                                
                      Lmgmed >= :Ocampa And                                                         
                      Lmgzon = :P_zon And                                                           
                      Gldsct = :wsec  And                                                           
                      Gldcte = :Wcte  And                                                           
                      Lmgnir in ('I','R') and                                                       
                      (GldCin = :WcampaX Or GldCin = :WcampaY) And                                  
                      Lmgtvp <> 0 and                                                               
                (Gldest = ' ' Or (Gldest = 'D' And (Gldcdt = :WcampaX Or                            
                 Gldcdt = :WcampaY Or Gldcdt >= :WcampaX)));                                        
         Orespuesta = 'N';                                                                          
        If Wnro >= Ynro Or w_IndNA = *Off;                                                          
         Orespuesta = 'S';                                                                          
        EndIf;                                                                                      
                                                                                                    
        Idfcia = p_cia;                                                                             
        Idfreg = Wregio;                                                                            
        Idfzng = p_zon;                                                                             
        Idfscg = Wsec;                                                                              
        Idfcdg = Wcte;                                                                              
        Idfcmp = p_cam;                                                                             
                                                                                                    
        WCedreempla = *Zeros;                                                                       
        WCedAux = *Zeros;                                                                           
                                                                                                    
       Exec Sql                                                                                     
        Select Rsgcgr Into :WCedreempla                                                             
        From Votrea00/Vtmrsgf                                                                       
        Where Rsgcia = :Idfcia And (Rsgtpo = ' ' Or Rsgtpo = 'P') And                               
              Rsgcga = :Idfcdg And Rsgzng = :Idfzng And Rsgscg = :Idfscg                            
              And :Idfcmp BetWeen Rsgcpi And Rsgcpf                                                 
        Order By Rsgcpi Asc                                                                         
        Fetch First 1 Row Only;                                                                     
                                                                                                    
       If WCedreempla > *Zeros;                                                                     
        WCedAux = Idfcdg;                                                                           
        Idfcdg = WCedreempla;                                                                       
       Endif;                                                                                       
                                                                                                    
        chain (idfcia:idfreg:idfzng:idfscg:idfcdg) vtwidff;                                         
         if not %found (vtwidff);                                                                   
           Idfnic = Ynro;                                                                           
           Idfisc = Orespuesta;                                                                     
           Idffec = %Char(%Date());                                                                 
           Idfhor = %Char(%Time());                                                                 
           Idfusr = %Trim(User);                                                                    
           write regidf;                                                                            
          else;                                                                                     
           Idfcmp = p_cam;                                                                          
           Idfnic = Ynro;                                                                           
           Idfisc = Orespuesta;                                                                     
           Idffec = %Char(%Date());                                                                 
           Idfhor = %Char(%Time());                                                                 
           Idfusr = %Trim(User);                                                                    
           update(E) regidf;                                                                        
         endif;                                                                                     
                                                                                                    
       If WCedreempla > *Zeros;                                                                     
        WCedreempla = *Zeros;                                                                       
        Idfcdg = WCedAux;                                                                           
        WCedAux = *Zeros;                                                                           
       Endif;                                                                                       
                                                                                                    
         If Idfisc ='N';                                                                            
          PrResumen();                                                                              
         EndIf;                                                                                     
         endif;                                                                                     
         endif;                                                                                     
                                                                                                    
        Exec SQL                                                                                    
         Fetch Cursor1 Into :Wcte, :Wzon,:Wsec,:Wtrr,:WcmpIni;                                      
       EndDo;                                                                                       
       *inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento que graba resumen en archivo Vttdngf               /                         
      //------------------------------------------------------------------/                         
     PPrResumen        B                                                                            
     DPrResumen        Pi                                                                           
      *Definición de variables de trabajo                                                           
     DWCedreempla      s              9  0  Inz(*Zeros)                                             
     DWCedAux          s              9  0  Inz(*Zeros)                                             
      /Free                                                                                         
                                                                                                    
        Clear Wdesc;                                                                                
        Exec SQL                                                                                    
        select dngdes into :wdesc                                                                   
        from votrea00/vtmdngf                                                                       
        where Dngcia = :P_cia And Dngcod = 5                                                        
        And Dngest = ' ';                                                                           
                                                                                                    
        If Wdesc <> ' ';                                                                            
        Dngcia = Idfcia;                                                                            
        Dngreg = Idfreg;                                                                            
        Dngzng = Idfzng;                                                                            
        Dngscg = Idfscg;                                                                            
        Dngcdg = Idfcdg;                                                                            
        Dngcmp = Idfcmp;                                                                            
        Wcod = 5;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Select Rsgcgr Into :WCedreempla                                                             
        From Votrea00/Vtmrsgf                                                                       
        Where Rsgcia = :Dngcia And (Rsgtpo = ' ' Or Rsgtpo = 'P') And                               
              Rsgcga = :Dngcdg And Rsgzng = :Dngzng And Rsgscg = :Dngscg                            
              And :Dngcmp BetWeen Rsgcpi And Rsgcpf                                                 
        Order By Rsgcpi Asc                                                                         
        Fetch First 1 Row Only;                                                                     
                                                                                                    
       If WCedreempla > *Zeros;                                                                     
        WCedAux = Dngcdg;                                                                           
        Dngcdg = WCedreempla;                                                                       
       Endif;                                                                                       
                                                                                                    
        Chain (dngcia:dngreg:dngzng:dngscg:dngcdg:dngcmp:Wcod) Vttdngf;                             
        if not %found(Vttdngf);                                                                     
         PrVlrPagar();                                                                              
         Dngest = 'E';                                                                              
         Dngvlr = WDngvlr;                                                                          
         Dngcdr = Wcod;                                                                             
         Dngmot = wdesc;                                                                            
         Dngaut = *blanks;                                                                          
         Dngfec = %Char(%Date());                                                                   
         Dnghor = %Char(%Time());                                                                   
         Dngusr = %Trim(User);                                                                      
         write Regdng;                                                                              
                                                                                                    
                                                                                                    
         PcgCia = dngcia;                                                                           
         PcgReg = dngreg;                                                                           
         PcgZna = dngzng;                                                                           
         PcgSct = dngscg;                                                                           
         PcgCmp = dngcmp;                                                                           
         PcgCdg = dngcdg;                                                                           
         PcgCdp = WCod;                                                                             
         PcgInd = 'N';                                                                              
         PcgDes = wdesc;                                                                            
         PcgFec = %Char(%Date);                                                                     
         PcgHor = %Char(%Time);                                                                     
         PcgUsr = %Trim(User);                                                                      
         Write(E) RegPcg;                                                                           
        else;                                                                                       
         if Dngest ='E';                                                                            
          PrVlrPagar();                                                                             
          Dngvlr = WDngvlr;                                                                         
          Dngmot = wdesc;                                                                           
          Dngaut = *blanks;                                                                         
          Dngfec = %Char(%Date());                                                                  
          Dnghor = %Char(%Time());                                                                  
          Dngusr = %Trim(User);                                                                     
          Update Regdng;                                                                            
         endif;                                                                                     
       endif;                                                                                       
                                                                                                    
       If WCedreempla > *Zeros;                                                                     
        WCedreempla = *Zeros;                                                                       
        Dngcdg = WCedAux;                                                                           
        WCedAux = *Zeros;                                                                           
       Endif;                                                                                       
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrResumen        E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Recupera Valor a pagar                              //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrVlrPagar       B                                                                            
     DPrVlrPagar       Pi                                                                           
      *Definición de variables de trabajo                                                           
     DWSQL             s           2000a    Inz(*Blanks)                                            
     D$COMA            C                   ''''                                                     
      /Free                                                                                         
       XValor = *Zeros;                                                                             
       Wsql = *Blanks;                                                                              
       Wsql = 'Select IfNull(SpgVps, 0) ' +                                                         
              'From VotreA00/VtmSpgf ' +                                                            
              'Where Spgcia= ' + $Coma + %trim(P_Cia) + $Coma + ' And ' +                           
              'SpgZon = ' + $Coma + %trim(P_Zon) + $Coma + ' And ' +                                
              'Spgcam = ' + $Coma + %trim(P_Cam) + $Coma + ' And ' +                                
              'Spgsec = ' + $Coma + %trim(Dngscg)  + $Coma + ' And ' +                              
              'Spgced = ' + %trim(%char(Dngcdg)) + ' And ' +                                        
              'SpgTrr = '' ''';                                                                     
       Exec Sql                                                                                     
         Prepare Cursor3 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor3 Scroll Cursor For Cursor3;                                                 
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor3;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor3 Into :XValor;                                                                
                                                                                                    
       WDngvlr = *Zeros;                                                                            
       IF sqlcod=*Zeros;                                                                            
       WDngvlr = XValor;                                                                            
       EndIF;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close cursor3;                                                                             
      /End-Free                                                                                     
     PPrVlrPagar       E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento que Valida el Numero de Inscripciones de la Guia    //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrInsGuiasNueva  B                                                                            
     DPrInsGuiasNueva  Pi              n                                                            
      *Definición de variables de trabajo                                                           
     DTsw              s               n    Inz(*Off)                                               
     DOcampa           s              4a    Inz(*Blanks)                                            
      /Free                                                                                         
        Ocampa = *Blanks;                                                                           
        PCamE=WcampaX;                                                                              
        PCamR=*Blanks;                                                                              
        POper='-';                                                                                  
        Pnume='002';                                                                                
        proCampa(P_Cia:PCamE:PCamR:POper:Pnume);                                                    
        If PCamR = '2009';                                                                          
         PCamR = '2008';                                                                            
        Endif;                                                                                      
        Ocampa = PCamR;                                                                             
        If WcmpIni >= Ocampa;                                                                       
        Wnro = *Zeros;                                                                              
        Exec SQL                                                                                    
           Select count(0) into :Wnro                                                               
            From Votrea00/Vtglmgf Inner Join Votrea00/Vtmgldf On                                    
                  Lmgcia = Gldcia And                                                               
                  Lmgzon = Gldzon And                                                               
                  Lmgsec = Gldsct                                                                   
           Where Lmgcia = :P_cia And                                                                
                      Lmgmed >= :Ocampa And                                                         
                      Lmgzon = :P_zon And                                                           
                      Gldsct = :wsec And                                                            
                      Gldcte = :Wcte And                                                            
                      Lmgmed >= Gldcin And                                                          
                      Gldcin >= :Ocampa And                                                         
                      Lmgnir in ('I','R') And                                                       
                      Lmgtvp <> 0 And                                                               
                (Gldest = ' ' Or (Gldest = 'D' And (Gldcdt = :WcampaX Or                            
                 Gldcdt = :WcampaY Or Gldcdt >= :WcampaX)));                                        
                                                                                                    
        If Wnro > *Zeros;                                                                           
         Tsw = *On;                                                                                 
         w_IndNA = *On;                                                                             
        Endif;                                                                                      
        Else;                                                                                       
         Tsw = *On;                                                                                 
         w_IndNA = *On;                                                                             
        Endif;                                                                                      
        Return Tsw;                                                                                 
      /End-Free                                                                                     
     PPrInsGuiasNueva  E                                                                            
