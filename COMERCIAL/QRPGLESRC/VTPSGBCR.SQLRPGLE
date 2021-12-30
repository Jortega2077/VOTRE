      **********************************************************************                        
      ** Programa     : VTPSGBCR                                          **                        
      ** Descripciòn: : Pg Clasificacón Nivel de Alas Baja                **                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Edgar Ivan Maldonado Perez - PersonalSoft         **                        
      ** Fecha Creaciòn: 1 de Junio de 2016                               **                        
      **********************************************************************                        
     HOption(*NoDebugIo)                                                                            
     HDFTACTGRP(*NO)                                                                                
                                                                                                    
     ** Definiciòn de Archivo de Trabajo                                                            
     FVtmcnaf   Uf A e           k Disk                                                             
                                                                                                    
      *Definición de variables de trabajo                                                           
     Dcia              s              3a                                                            
     DXcia             s              3    Inz(*Blanks)                                             
     DPres             s              2a   Inz(*Blanks)                                             
     DWsql             S           1000A                                                            
     D$COMA            C                   ''''                                                     
                                                                                                    
     DFields         e Ds                  Extname(Vtmcnaf:Regcna)                                  
     D                                     Prefix(Q)                                                
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DResulCond        Ds                  Occurs(999) Qualified                                    
     DOCia                            3                                                             
     DOCodig                         15  0                                                          
     DODescr                         50                                                             
     DOcond                          50                                                             
     DSuperr                         10  0                                                          
     DInferr                         10  0                                                          
     DTpo                             1                                                             
                                                                                                    
      * Definir Vectores                                                                            
     D VecAlas         S              4  0 DIM(99)                                                  
                                                                                                    
      *Definición de Procedimientos                                                                 
     DPintarfile       Pr                                                                           
                                                                                                    
     DCompaÑia         Pr                  Extpgm('VOTREP00/VTMCSBFR')                              
     DUcia                           10                                                             
     Dkres                            2                                                             
     DkciaR                          10                                                             
      * Programa validar region                                                                     
     DPgmregio         Pr                  Extpgm('VOTREP00/VTMGOIGR')                              
     DIcia                            3                                                             
     DIzona                           3                                                             
     DIRegion                        30                                                             
     DXced                           10                                                             
     DXplan                          10                                                             
                                                                                                    
                                                                                                    
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPCia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
                                                                                                    
     DPgmCampaÑa       Pr                  Extpgm('VOTREP00/VTMPGB7R')                              
     DICia                            3                                                             
     DIRegion                        30                                                             
     DIZona                           3                                                             
     DISector                         3                                                             
     DICampa                          4                                                             
     DICedeulaG                       9  0                                                          
     DIId                             1                                                             
     DOnro                            4  0                                                          
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DIcia                            3a                                                            
     DICamPa                          4A                                                            
     DIZona                           3a                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DIcia                            3a                                                            
     DICamPa                          4A                                                            
     DIZona                           3a                                                            
                                                                                                    
     ** Definición de Variables de Trabajo                                                          
     DW_User           s             10a   Inz(*User)                                               
     DwGldcia          s              3    Inz(*Blanks)                                             
     DIRegion          s             30    Inz(*Blanks)                                             
     DISector          s              3    Inz(*Blanks)                                             
     DICedeulaG        s              9  0 Inz(*Zeros)                                              
     DIId              s              1    Inz(*Blanks)                                             
     DOnro             s              4  0 Inz(*Zeros)                                              
     DWcia             s              3    Inz(*Blanks)                                             
     DWzona            s              3    Inz(*Blanks)                                             
     DWSect            s              3    Inz(*Blanks)                                             
     DWCdGui           s             15  0 Inz(*Zeros)                                              
     DXced             s             10    Inz(*Blanks)                                             
     DXplan            s             10    Inz(*Blanks)                                             
     DWTotal           s             15  0 Inz(*Zeros)                                              
     DPCia             s              3a   Inz(*Blanks)                                             
     D Idx             s             10i 0                                                          
     DPCamE            s              4a   Inz(*Blanks)                                             
     DPCamR            s              4a   Inz(*Blanks)                                             
     DPOper            s              1a   Inz(*Blanks)                                             
     DPnume            s              3a   Inz(*Blanks)                                             
     DWCampa           s              4a   Inz(*Blanks)                                             
     DWTagcia          s              3a   Inz(*Blanks)                                             
     DWTagcod          s              4  0 Inz(*Zeros)                                              
     DWTagdes          s             50a   Inz(*Blanks)                                             
     DWTagcnd          s            100a   Inz(*Blanks)                                             
     DWnro             s              4  0 Inz(*Zeros)                                              
     DYnro             s              4  0 Inz(*Zeros)                                              
     DWprueba          s             50a   Inz(*Blanks)                                             
     DWEDECDG          s              9  0 Inz(*Zeros)                                              
     D CONTA           s              4  0 Inz(*Zeros)                                              
     DPDESW            s             50a   Inz(*Blanks)                                             
     DPOSVEC           s              2  0 Inz(*Zeros)                                              
     DTAGCODW          s              4  0 Inz(*Zeros)                                              
     DTAGDESW          s             50a   Inz(*Blanks)                                             
     DPos1             s              2  0                                                          
     DDato1            S             10A                                                            
     DDato2            S             10A                                                            
     DW_Tipo           S              1A                                                            
     DWcampaX          S              4a   Inz(*Blanks)                                             
     DWcampaY          S              4a   Inz(*Blanks)                                             
                                                                                                    
     DDato1n           S              9  0                                                          
     DDato2n           S              9  0                                                          
     Dwincre           s              3  0 Inz(*Zeros)                                              
     DWcount           s              3  0 Inz(*Zeros)                                              
     DWcount1          s              3  0 Inz(*Zeros)                                              
     DYEDENRO          s              9  0 Inz(*Zeros)                                              
     DTEDENRO          s              9  0 Inz(*Zeros)                                              
     DIcaso            s              7a   Inz(*Blanks)                                             
     DIfecha           s             10a   Inz(*Blanks)                                             
     DIcedula          s             30a   Inz(*Blanks)                                             
     DOrespuesta       s              1a   Inz(*Blanks)                                             
     DWCedreempla      s              9  0  Inz(*Zeros)                                             
     DWCedAux          s              9  0  Inz(*Zeros)                                             
                                                                                                    
     DPgmNewPlanG      Pr                  Extpgm('VOTREP00/VTMPGB0R')                              
     DPcia                            3a                                                            
     DIzona                           3a                                                            
     DIcaso                           7a                                                            
     DIcedula                        30a                                                            
     DIfecha                         10a                                                            
     DOrespuesta                      1a                                                            
      //////////////////////////////////////////////////////////////////////                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /Free                                                                                         
       // Se cambia calculo NO con la Campaña Actual - 1 si no con la campaña                       
       // Actual de Cierre de Zona fecha 02-11-2016, Asi se Solicito en el                          
       // Requerimiento.                                                                            
       WcampaY = ICamPa;                                                                            
       WcampaX = *Blanks;                                                                           
       //Invocar programa para calculo de campaña anterior                                          
       PCamE=ICampa;                                                                                
       PCamR=*Blanks;                                                                               
       POper='-';                                                                                   
       Pnume='001';                                                                                 
       If ICampa = '2010';                                                                          
        Pnume = '002';                                                                              
       Endif;                                                                                       
       proCampa(Icia:PCamE:PCamR:POper:Pnume);                                                      
       WcampaX = PCamR;                                                                             
          PrCarVecTagcnd();                                                                         
       // Se invoca el programa VTMGOIGR para tomar la Región                                       
           CallP(e) Pgmregio(Icia:Izona:IRegion:Xced:Xplan);                                        
                                                                                                    
           Exec Sql                                                                                 
              Declare CursorG Cursor For                                                            
              Select Gldcia, GldZon, Gldsct, Gldcte                                                 
              From Votrea00/Vtmgldf                                                                 
              Where Gldcia = :ICia                                                                  
              And GldZon = :IZona                                                                   
              And ((Gldest = ' ' And Gldcin <= :WcampaY) Or                                         
            (Gldest = 'D' And Gldcin <= :PcamR And (Gldcdt = :WcampaX                               
            Or Gldcdt = :WcampaY Or Gldcdt >= :WcampaX)))                                           
            And Gldsct <> ' ';                                                                      
                                                                                                    
           Exec Sql                                                                                 
              Open cursorG;                                                                         
                                                                                                    
           Exec Sql                                                                                 
              Fetch CursorG Into :Wcia, :Wzona, :WSect, :WCdGui;                                    
         Dow Sqlcode = *Zeros;                                                                      
                                                                                                    
          WCedreempla = *Zeros;                                                                     
          WCedAux = *Zeros;                                                                         
                                                                                                    
          Exec Sql                                                                                  
           Select Rsgcgr Into :WCedreempla                                                          
           From Votrea00/Vtmrsgf                                                                    
           Where Rsgcia = :Wcia And (Rsgtpo = ' ' Or Rsgtpo = 'P') And                              
                 Rsgcga = :WCdGui And Rsgzng = :Wzona And Rsgscg = :WSect                           
                 And :ICamPa BetWeen Rsgcpi And Rsgcpf                                              
           Order By Rsgcpi Asc                                                                      
           Fetch First 1 Row Only;                                                                  
                                                                                                    
           If WCedreempla > *Zeros;                                                                 
            WCedAux = WCdGui;                                                                       
            WCdGui = WCedreempla;                                                                   
           Endif;                                                                                   
                                                                                                    
          // Se invoca el programa                                                                  
          IId='B';                                                                                  
          ICedeulaG = WCdGui;                                                                       
          Callp PgmCampaÑa(ICia:IRegion:IZona:WSect:ICampa:                                         
                           ICedeulaG:IId:Onro);                                                     
          //Nro de campañas para clasificacion de pedidos estrella por guia                         
           Clear Ynro;                                                                              
           Exec SQL                                                                                 
            select Count(0) into :Ynro                                                              
            from Votrea00/Vtwedef                                                                   
            where edecia = :Wcia and                                                                
                  edezna = :Wzona and                                                               
                  edesct = :Wsect and                                                               
                  edecdg = :WCdGui;                                                                 
          // condicion de grabado / actualizacion archivo                                           
           Icaso = *Blanks;                                                                         
           Icedula = %Trim(%Editc(WCdGui:'Z'));                                                     
           Ifecha = *Blanks;                                                                        
           Orespuesta = *Blanks;                                                                    
           PgmNewPlanG(Icia:Izona:Icaso:Icedula:Ifecha:Orespuesta);                                 
           If Orespuesta = 'S';                                                                     
            WTotal = 0;                                                                             
            WCampa = ICampa;                                                                        
            For Idx= 1 To Onro;                                                                     
             PrPediEstrellxG();                                                                     
             //Invocar programa para calculo de campaña anterior                                    
             PCamE=Icampa;                                                                          
             PCamR=*Blanks;                                                                         
             POper='-';                                                                             
             Pnume=%char(Idx);                                                                      
             If Icampa = '2010' And Idx = 1;                                                        
              Pnume = '002';                                                                        
             Endif;                                                                                 
             proCampa(Icia:PCamE:PCamR:POper:Pnume);                                                
             WCampa = PCamR;                                                                        
            EndFor;                                                                                 
            PrcomparaRango();                                                                       
            PrCreaRegistro();                                                                       
           Endif;                                                                                   
                                                                                                    
           If WCedreempla > *Zeros;                                                                 
            WCedreempla = *Zeros;                                                                   
            WCdGui = WCedAux;                                                                       
            WCedAux = *Zeros;                                                                       
           Endif;                                                                                   
                                                                                                    
           Exec Sql                                                                                 
               Fetch CursorG Into :Wcia, :Wzona, :WSect, :WCdGui;                                   
         EndDo;                                                                                     
                                                                                                    
       Exec SQL                                                                                     
        Close CursorG;                                                                              
                                                                                                    
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento Determina el Nro de Campañas Que se ha realizado  //                         
      // la clasificación de pedidos estrella de cada Guía               //                         
      //------------------------------------------------------------------/                         
     PPrPediEstrellxG  B                                                                            
     DPrPediEstrellxG  Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
             WEDECDG = WCdGui;                                                                      
        Clear Wnro;                                                                                 
        Exec Sql                                                                                    
          Select EDENRO  Into :Wnro                                                                 
          from VotreA00/Vtwedef                                                                     
          Where EDECIA = :ICia                                                                      
          and EDEZNA = :Izona                                                                       
          and EDECMP = :WCampa                                                                      
          and EDESCT = :WSect                                                                       
          and EDECDG = :WEDECDG;                                                                    
                                                                                                    
                                                                                                    
          WTotal += Wnro;                                                                           
      /End-Free                                                                                     
     PPrPediEstrellxG  E                                                                            
                                                                                                    
      //-----------------------------------------------------------------//                         
      // Procedimiento Carga Condición del archivo Maestro Clalificación //                         
      // Alas, donde el estado seá vacio                                 //                         
      //-----------------------------------------------------------------//                         
     PPrCarVecTagcnd   B                                                                            
     DPrCarVecTagcnd   Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
           Exec Sql                                                                                 
              Declare CursorTag Cursor For                                                          
              Select Tagcia, Tagcod, Tagdes, Tagcnd                                                 
              From Votrea00/Vtmtagf                                                                 
              Where Tagest = ' '                                                                    
              AND Tagcia= :ICia                                                                     
              ORDER BY Tagcod DESC;                                                                 
                                                                                                    
           Exec Sql                                                                                 
              Open cursorTag;                                                                       
                                                                                                    
           Exec Sql                                                                                 
              Fetch CursorTag Into :WTagcia, :WTagcod, :WTagdes, :WTagcnd;                          
                                                                                                    
         Dow  Sqlcode = *Zeros;                                                                     
            PrCAlRango();                                                                           
           Exec Sql                                                                                 
              Fetch CursorTag Into :WTagcia, :WTagcod, :WTagdes, :WTagcnd;                          
         EndDo;                                                                                     
                                                                                                    
       Exec SQL                                                                                     
        Close CursorTag;                                                                            
      /End-Free                                                                                     
     PPrCarVecTagcnd   E                                                                            
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento Carga Condición del archivo Maestro Clalificación //                         
      // Alas, donde el estado seá vacio                                 //                         
      //------------------------------------------------------------------/                         
     PPrCAlRango       B                                                                            
     DPrCAlRango       Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
       Pos1 = %Scan('-':WTagcnd);                                                                   
       If pos1 <> *Zeros;                                                                           
         W_Tipo = '-';                                                                              
       Else;                                                                                        
         Pos1 = %Scan('+':WTagcnd);                                                                 
         W_Tipo = '+';                                                                              
       EndIf;                                                                                       
       Dato1 = %SubSt(WTagcnd: 1: Pos1 - 1);                                                        
       Dato2 = %SubSt(WTagcnd: Pos1 + 1 : %len(%Trim(WTagcnd)));                                    
       Monitor;                                                                                     
         Dato1n = %Int(Dato1);                                                                      
       On-Error;                                                                                    
         Clear Dato1n;                                                                              
       EndMon;                                                                                      
                                                                                                    
       Monitor;                                                                                     
         Dato2n = %Int(Dato2);                                                                      
       On-Error;                                                                                    
         Clear Dato2n;                                                                              
       EndMon;                                                                                      
                                                                                                    
        wincre += 1;                                                                                
            %Occur(ResulCond) = wincre;                                                             
            ResulCond.OCia   = WTagcia;                                                             
            ResulCond.OCodig = WTagcod;                                                             
            ResulCond.ODescr = WTagdes;                                                             
            ResulCond.Ocond  = WTagcnd;                                                             
            ResulCond.Inferr = Dato1n;                                                              
            ResulCond.Superr = Dato2n;                                                              
            ResulCond.Tpo    = W_Tipo;                                                              
                                                                                                    
      /End-Free                                                                                     
     PPrCAlRango       E                                                                            
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento Asigna codigo                                     //                         
      //------------------------------------------------------------------/                         
     PPrcomparaRango   B                                                                            
     DPrcomparaRango   Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
        For Wcount=1 to wincre;                                                                     
            %Occur(ResulCond) = Wcount;                                                             
            If ResulCond.Tpo = '-';                                                                 
              If WTotal  >= ResulCond.Inferr  And  WTotal <= ResulCond.Superr;                      
            VecAlas(ResulCond.OCodig)=ResulCond.OCodig;                                             
            PosVec=ResulCond.OCodig;                                                                
            PdesW = ResulCond.ODescr;                                                               
              Wcount = wincre;                                                                      
              EndIf;                                                                                
            Else;                                                                                   
            if WTotal  >= ResulCond.Inferr;                                                         
            VecAlas(ResulCond.OCodig)=ResulCond.OCodig;                                             
            PosVec=ResulCond.OCodig;                                                                
            PdesW = ResulCond.ODescr;                                                               
              Wcount = wincre;                                                                      
              EndIf;                                                                                
            EndIf;                                                                                  
        EndFor;                                                                                     
      /End-Free                                                                                     
     PPrcomparaRango   E                                                                            
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento Carga Campos Crea Registros                       //                         
      //------------------------------------------------------------------/                         
     PPrCreaRegistro   B                                                                            
     DPrCreaRegistro   Pi                                                                           
      /Free                                                                                         
        Chain (ICia:IRegion:IZona:ICampa:WSect:WCdGui) VTmcnaf;                                     
         If Not %Found(VTmcnaf);                                                                    
         Cnacia = ICia   ;                                                                          
         Cnareg = IRegion;                                                                          
         Cnazna = IZona  ;                                                                          
         CnaSec = WSect;                                                                            
         Cnacmp = ICampa ;                                                                          
         Cnacdg = WCdGui;                                                                           
         Cnatpo = *Blanks;                                                                          
         Cnanpe = WTotal ;                                                                          
         Cnance = Onro   ;                                                                          
         Cnacod = PosVec ;                                                                          
         Cnades = PdesW  ;                                                                          
         Cnafec = %Char(%Date());                                                                   
         Cnahor = %Char(%Time());                                                                   
         Cnausr = %Trim(W_USer);                                                                    
         Cnaest = ' ';                                                                              
             Write(E) RegCna;                                                                       
          Clear WTotal;                                                                             
          Else;                                                                                     
         If Cnatpo = ' ' Or Cnatpo = 'B';                                                           
          If Cnacod <> PosVec;                                                                      
           If Ynro <> Onro;                                                                         
            Cnatpo = 'B';                                                                           
           Else;                                                                                    
            Cnatpo = *Blanks;                                                                       
           Endif;                                                                                   
           Cnacod = PosVec ;                                                                        
           Cnanpe = WTotal ;                                                                        
           Cnance = Onro   ;                                                                        
           Cnacod = PosVec ;                                                                        
           Cnades = PdesW  ;                                                                        
           Cnafec = %Char(%Date());                                                                 
           Cnahor = %Char(%Time());                                                                 
           Cnausr = %Trim(W_USer);                                                                  
           UpDate  RegCna;                                                                          
           Clear WTotal;                                                                            
          Endif;                                                                                    
         Endif;                                                                                     
        Endif;                                                                                      
      /End-Free                                                                                     
     PPrCreaRegistro   E                                                                            
