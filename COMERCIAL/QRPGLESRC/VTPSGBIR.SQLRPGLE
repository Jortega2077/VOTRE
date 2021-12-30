      **********************************************************************                        
      ** Programa     : VTPSGBIR                                          **                        
      ** Descripciòn: : Pg Liquida Bono a la Guía si el Foco es Conservar **                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                        
      ** Fecha Creaciòn: 10 de Junio de 2016                              **                        
      **********************************************************************                        
     HOption(*NoDebugIo)                                                                            
     HDFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('QC2LE')                                                   
                                                                                                    
     ** Definición de Prototipos de Trabajo                                                         
     FVttpgbf   UF A E           K DISK                                                             
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DIcia                            3a                                                            
     DICamPa                          4A                                                            
     DIZona                           3a                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DIcia                            3a                                                            
     DICamPa                          4A                                                            
     DIZona                           3a                                                            
                                                                                                    
                                                                                                    
      *Definición de Procedimientos                                                                 
                                                                                                    
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPCia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
                                                                                                    
      * Programa validar region                                                                     
     DPgmregio         Pr                  Extpgm('VOTREP00/VTMGOIGR')                              
     DIcia                            3a                                                            
     DIzona                           3a                                                            
     DORegion                        30a                                                            
     DOced                           10A                                                            
     DOplan                          10a                                                            
                                                                                                    
      * Programa validar region                                                                     
     DPgmVlrPgrBono    Pr                  Extpgm('VOTREP00/VTMPGBGR')                              
     DIcia                            3a                                                            
     DIregion                        30a                                                            
     DIzona                           3a                                                            
     DIsector                         3a                                                            
     DIcampa                          4a                                                            
     DIfoco                           1a                                                            
     DNomFile                        10a                                                            
                                                                                                    
     ** Definiciòn de Vectores de Trabajo                                                           
     DString           s              1    Dim(99)                                                  
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DCaracteres       Ds            12                                                             
     DVpermitidos                     1    Dim(12)                                                  
     D                                     Overlay(Caracteres)                                      
     DResulNomFile     Ds                  Occurs(99) Qualified                                     
     DWCondic                         5  2 Inz(*Zeros)                                              
     DW_valor                        11  2 Inz(*Zeros)                                              
     DWDesCondi                     200a   Inz(*Blanks)                                             
                                                                                                    
      *Definición de variables de trabajo                                                           
                                                                                                    
     DPCamE            S              4a   Inz(*Blanks)                                             
     DPCamR            S              4a   Inz(*Blanks)                                             
     DPOper            S              1a   Inz(*Blanks)                                             
     DPnume            S              3a   Inz(*Blanks)                                             
     DWsql             S           1000a   Inz(*Blanks)                                             
     D$COMA            C                   ''''                                                     
     DWGldcte          S              9  0 Inz(*Zeros)                                              
     DWGldzon          S              3a   Inz(*Blanks)                                             
     DWGldsct          S              3a   Inz(*Blanks)                                             
     DWGldtrr          S              1a   Inz(*Blanks)                                             
     DWvalor           S             15  0 Inz(*Zeros)                                              
     DNpdConse         S              5  0 Inz(*Zeros)                                              
     DWNpdConse        S              5  0 Inz(*Zeros)                                              
     DWNCmSector       S              5  0 Inz(*Zeros)                                              
     DNCmSector        S              5  0 Inz(*Zeros)                                              
     DPorCe            s              5  2 Inz(*Zeros)                                              
     DORegion          s             30A   Inz(*Blanks)                                             
     DOCed             s             10A   Inz(*Blanks)                                             
     DOplan            s             10a   Inz(*Blanks)                                             
     DIfoco            s              1a   Inz(*Blanks)                                             
     DNomFile          s             10a   Inz(*Blanks)                                             
     DWNF_Condicion    s            200a   Inz(*Blanks)                                             
     DWNF_Valor        s             11a   Inz(*Blanks)                                             
     DDato1            s             50a   Inz(*Blanks)                                             
     DWCND             s              3  0 Inz(*Zeros)                                              
     DPos1             s              5  0 Inz(*Zeros)                                              
     DPos2             s              5  0 Inz(*Zeros)                                              
     DDato1n           s              5  2 Inz(*Zeros)                                              
     DDato2n           s             11  2 Inz(*Zeros)                                              
     DWvlrcond         s              5  0 Inz(*Zeros)                                              
     DWpgbvlr          s             11  2 Inz(*Zeros)                                              
     DWPgbcon          s            200a   Inz(*Blanks)                                             
     DStringIO         s           1664a                                                            
     DIdx              s              4s 0 Inz(*Zeros)                                              
     DXString          s            200a   Inz(*Blanks)                                             
     Dvarp             s             11  2 Inz(*Zeros)                                              
     DW_USER           s             10a   Inz(*User)                                               
     DWporceCond       s              5  2 Inz(*Zeros)                                              
     DWPosConMx        s              5  0 Inz(*Zeros)                                              
     DW_Reg            s            100A                                                            
     DW_Cia            s              3a                                                            
     DW_Zona           s              3A                                                            
     DW_Camp           s              4A                                                            
     DW_foco           s              1A                                                            
     DIcaso            s              7a   Inz(*Blanks)                                             
     DIfecha           s             10a   Inz(*Blanks)                                             
     DIcedula          s             30a   Inz(*Blanks)                                             
     DOrespuestaP      s              1a   Inz(*Blanks)                                             
     DXNF_NroFoco      s             15s 2 Inz(*Zeros)                                              
     DWNF_NroFoco      s             15s 2 Inz(*Zeros)                                              
     DWCedreempla      s              9  0  Inz(*Zeros)                                             
     DWCedAux          s              9  0  Inz(*Zeros)                                             
     DPCamEa           s              4a   Inz(*Blanks)                                             
     DPCamRa           S              4a   Inz(*Blanks)                                             
     DPOpera           S              1a   Inz(*Blanks)                                             
     DPnumea           S              3a   Inz(*Blanks)                                             
     DKcam             s              4a                                                            
     DTfecha           s             10a   Inz(*Blanks)                                             
       Dcl-s PciaV Char(3) Inz('999');                                                              
       Dcl-s Xrol Char(2) Inz('CM');                                                                
       Dcl-s Xfec Char(10) Inz(*Blanks);                                                            
       Dcl-s Xtexto Char(200) Inz(*Blanks);                                                         
                                                                                                    
     DPgmNewPlanG      Pr                  Extpgm('VOTREP00/VTMPGB0R')                              
     DPcia                            3a                                                            
     DIzona                           3a                                                            
     DIcaso                           7a                                                            
     DIcedula                        30a                                                            
     DIfecha                         10a                                                            
     DOrespuestaP                     1a                                                            
                                                                                                    
       Dcl-Pr PgmMigracion Extpgm('VOTREP00/VTPLDITR');                                             
        Xcia Char(3);                                                                               
        Xrol Char(2);                                                                               
        Xfec Char(10);                                                                              
        Xtexto Char(200);                                                                           
       End-Pr;                                                                                      
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /Free                                                                                         
        //Resta de la  Campaña Cierre de Zona Menos 1 Campañas                                      
           PCamE = ICamPa;                                                                          
           PCamR = *Blanks;                                                                         
           POper = '-';                                                                             
           Pnume = '1';                                                                             
           If ICampa = '2010';                                                                      
            Pnume = '002';                                                                          
           Endif;                                                                                   
           callp(E) ProCampa(Icia:PCamE:PCamR:POper:Pnume);                                         
           PrValidFoco();                                                                           
       If XNF_NroFoco > *Zeros;                                                                     
       Wsql = *Blanks;                                                                              
       Wsql = 'SELECT Gldcte, Gldzon, Gldsct, Gldtrr ' +                                            
              'FROM VotreA00/Vtmgldf '+                                                             
              'WHERE Gldcia = ' + $Coma + %trim(Icia) + $Coma + ' And ' +                           
              'GLDZON = ' + $Coma + %trim(IZona) + $Coma + ' And ' +                                
              '((GLDEST = '' '' ' + ' And Gldcin <= ' + '''' + PCamR +                              
              '''' + ') Or (GLDEST = ''D'' And Gldcin <= ' + '''' + PCamR +                         
              '''' + ' And (' +                                                                     
              ' Gldcdt = ' +  $Coma + %trim(PCamR) + $Coma + ' Or ' +                               
              ' Gldcdt = ' +  $Coma + %trim(Icampa) + $Coma + ' Or ' +                              
              ' Gldcdt >= ' +  $Coma + %trim(PCamR) + $Coma + ')))' +                               
              ' And Gldsct <> ' + '''' + ' ' + '''';                                                
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor1 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor1 Scroll Cursor For Cursor1;                                                 
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor1;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor1 Into :WGldcte, :WGldzon, :WGldsct, :WGldtrr;                                 
                                                                                                    
        Dow sqlcod=*Zeros;                                                                          
                                                                                                    
                                                                                                    
         WCedreempla = *Zeros;                                                                      
         WCedAux = *Zeros;                                                                          
                                                                                                    
         Exec Sql                                                                                   
          Select Rsgcgr Into :WCedreempla                                                           
          From Votrea00/Vtmrsgf                                                                     
          Where Rsgcia = :Icia And (Rsgtpo = ' ' Or Rsgtpo = 'P') And                               
                Rsgcga = :WGldcte And Rsgzng = :Izona And Rsgscg = :WGldsct                         
                And :PCamR BetWeen Rsgcpi And Rsgcpf                                                
          Order By Rsgcpi Asc                                                                       
          Fetch First 1 Row Only;                                                                   
                                                                                                    
          If WCedreempla > *Zeros;                                                                  
           WCedAux = WGldcte;                                                                       
           WGldcte = WCedreempla;                                                                   
          Endif;                                                                                    
                                                                                                    
         Icaso = *Blanks;                                                                           
         Icedula = %Trim(%Editc(WGldcte:'Z'));                                                      
         Ifecha = '777777';                                                                         
         OrespuestaP = *Blanks;                                                                     
         PgmNewPlanG(Icia:Izona:Icaso:Icedula:Ifecha:OrespuestaP);                                  
          Tfecha = %Char(%Date());                                                                  
          Kcam = *Blanks;                                                                           
          PgmMigracion(PciaV:Xrol:Xfec:Xtexto);                                                     
          If Xfec = *Blanks;                                                                        
           Exec Sql                                                                                 
            Select Max(Substr(Digits(Izpano), 3, 2) ||                                              
                   Digits(Izpcap)) Into :Kcam                                                       
            From Vpcopa00/Vtmizpf                                                                   
            Where Izpcia = :Icia And                                                                
                  Izpzon = '998' And                                                                
                  Substr(Char(Izpftp, Iso), 1, 4) || '-' ||                                         
                  Substr(Char(Izpftp, Iso), 6, 2) || '-' ||                                         
                  Substr(Char(Izpftp, Iso), 9, 2) <= :Ifecha                                        
             Fetch First 1 Row Only;                                                                
           Else;                                                                                    
            Exec Sql                                                                                
             Select Fcfcmp Into :Kcam                                                               
             From Vpcopa00/Vtcmfcf                                                                  
             Where Fcfcia = :Icia And                                                               
                   :Ifecha Between Fcffin And Fcfffn                                                
             Fetch First 1 Row Only;                                                                
           Endif;                                                                                   
          If Kcam <> *Blanks;                                                                       
           PCamEa = Kcam;                                                                           
           PCamRa = *Blanks;                                                                        
           POpera = '+';                                                                            
           Pnumea = '1';                                                                            
           If Kcam = '2008' And ICampa = '2010';                                                    
            Pnume = '002';                                                                          
           Endif;                                                                                   
           callp(E) ProCampa(Icia:PCamEa:PCamRa:POpera:Pnumea);                                     
           Kcam = PCamRa;                                                                           
          Endif;                                                                                    
           If PCamR >= Kcam;                                                                        
         If OrespuestaP = 'S';                                                                      
         // Colocar Archivo para Saber que tipo de Bono Aplica Jortega                              
          PrNpdConservar();                                                                         
          PrNCmSector();                                                                            
          If WNCmSector > *Zeros;                                                                   
           PorCe = (WNpdConse/WNCmSector) * 100;                                                    
          Else;                                                                                     
           PorCe = *Zeros;                                                                          
          Endif;                                                                                    
          // pgm retorna Región                                                                     
          CallP(E) Pgmregio(ICia:IZona:ORegion:Oced:Oplan);                                         
          IFoco = 'K';                                                                              
          NomFile = *Blanks;                                                                        
          Callp(E) PgmVlrPgrBono(ICia:ORegion:IZona:WGldsct:PCamR                                   
                                :IFoco:NomFile);                                                    
          PrFileQtemp();                                                                            
              pos2 = 1;                                                                             
          Dow pos2 <= pos1;                                                                         
          %Occur(ResulNomFile) =  pos2;                                                             
           If PorCe >= WPorceCond;                                                                  
             %Occur(ResulNomFile) =  WPosConMx;                                                     
            Wpgbvlr = ResulNomFile.W_valor;                                                         
            WpgbCon = ResulNomFile.WDesCondi;                                                       
            PrGrabar();                                                                             
              pos2 += pos1;                                                                         
           Else;                                                                                    
             Wvlrcond = ResulNomFile.WCondic;                                                       
             %Occur(ResulNomFile) =  pos2 + 1;                                                      
             If PorCe >= ResulNomFile.WCondic anD PorCe < Wvlrcond;                                 
              %Occur(ResulNomFile) =  pos2 + 1;                                                     
              Wpgbvlr = ResulNomFile.W_valor;                                                       
              WpgbCon = ResulNomFile.WDesCondi;                                                     
              PrGrabar();                                                                           
              pos2 += pos1;                                                                         
             EndIf;                                                                                 
           Endif;                                                                                   
              pos2 += 1;                                                                            
          EndDo;                                                                                    
         Endif;                                                                                     
                                                                                                    
         If WCedreempla > *Zeros;                                                                   
          WCedreempla = *Zeros;                                                                     
          WGldcte = WCedAux;                                                                        
          WCedAux = *Zeros;                                                                         
         Endif;                                                                                     
         Endif;                                                                                     
                                                                                                    
         Exec Sql                                                                                   
         fetch Cursor1 Into :WGldcte, :WGldzon, :WGldsct, :WGldtrr;                                 
        EndDo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor1;                                                                             
         Endif;                                                                                     
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Retorna el Nuemro de Compradoras con Pedidos        //                        
      //Consecutivos en la Campaña del Cierre de la Zona y Campaña Anterior/                        
      //////////////////////////////////////////////////////////////////////                        
     PPrNpdConservar   B                                                                            
     DPrNpdConservar   Pi                                                                           
      /Free                                                                                         
       Wsql = *Blanks;                                                                              
       Wsql = 'Select Count(0) ' +                                                                  
              'From Votrea00/Vtglmgf Inner Join Votrea00/Vtmgldf On ' +                             
              'Lmgcia=Gldcia And Lmgzon=Gldzon And Lmgsec = Gldsct ' +                              
              'Where Lmgcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'Lmgmed = ' + $Coma + %trim(PCamR) + $Coma + ' And ' +                                
              'Gldzon = ' + $Coma + %trim(IZona) + $Coma + ' And ' +                                
              'Gldcte = ' + %trim(%char(WGldcte)) + ' And (' +                                      
              'Gldest = '' '' Or ('+                                                                
              'Gldcdt = ' + $Coma + %trim(PCamR) + $Coma + ' OR ' +                                 
              'Gldcdt = ' + $Coma + %trim(ICampa) + $Coma + ' OR ' +                                
              'Gldcdt >= ' + $Coma + %trim(PCamR) + $Coma + ')) And ' +                             
              'Lmgcla = ''PC''  And  Gldsct = ' +                                                   
              $Coma + %Trim(WGldsct) + $Coma + ' And ' +                                            
              'Gldsct = ' + $Coma + %trim(WGldsct) + $Coma;                                         
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor4 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor4 Scroll Cursor For Cursor4;                                                 
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor4;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor4 Into :NpdConse;                                                              
                                                                                                    
         WNpdConse=*Zeros;                                                                          
        If  SqlCod = 0;                                                                             
         WNpdConse=NpdConse;                                                                        
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor4;                                                                             
                                                                                                    
      /End-Free                                                                                     
     PPrNpdConservar   E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento ConSulta el nuemro de compradoras de la Zona y      //                        
      //del Sector                                                        //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrNCmSector      B                                                                            
     DPrNCmSector      Pi                                                                           
      /Free                                                                                         
       Wsql = *Blanks;                                                                              
       Wsql = 'Select Count(0) ' +                                                                  
              'From Votrea00/Vtglmgf Inner Join Votrea00/Vtmgldf On ' +                             
              'Lmgcia=Gldcia And Lmgzon=Gldzon And Lmgsec = Gldsct ' +                              
              'Where Lmgcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'Lmgmed = ' + $Coma + %trim(PCamR) + $Coma + ' And ' +                                
              'Gldzon = ' + $Coma + %trim(IZona) + $Coma + ' And ' +                                
              'Gldcte = ' + %trim(%char(WGldcte)) + ' And (' +                                      
              'Gldest = '' '' Or ('+                                                                
              'Gldcdt = ' + $Coma + %trim(PCamR) + $Coma + ' OR ' +                                 
              'Gldcdt = ' + $Coma + %trim(ICampa) + $Coma + ' OR '  +                               
              'Gldcdt >= ' + $Coma + %trim(PCamR) + $Coma + ')) And '  +                            
              'Gldsct = ' + $Coma + %trim(WGldsct) + $Coma;                                         
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor5 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor5 Scroll Cursor For Cursor5;                                                 
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor5;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor5 Into :NCmSector;                                                             
                                                                                                    
         WNCmSector = *Zeros;                                                                       
        If SqlCod = 0;                                                                              
         WNCmSector = NCmSector;                                                                    
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor5;                                                                             
                                                                                                    
      /End-Free                                                                                     
     PPrNCmSector      E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Busca Rango del porcentage en el archivo creado,    //                        
      //el que retorna en la variable NomFile en la libreria QTEMP        //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrFileQtemp      B                                                                            
     DPrFileQtemp      Pi                                                                           
      /Free                                                                                         
       Wsql = *Blanks;                                                                              
       Wsql = 'Select Condicion, Valor ' +                                                          
              'From Qtemp/' + %trim(NomFile) + ' ' +                                                
              'Order By Rrn(' + %Trim(NomFile) + ') Desc';                                          
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor6A From :Wsql;                                                               
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor6A Scroll Cursor For Cursor6A;                                               
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor6A;                                                                             
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor6A Into :WNF_Condicion, :WNF_Valor;                                            
                                                                                                    
        WPosConMx = *Zeros;                                                                         
        WporceCond = *Zeros;                                                                        
        pos1  = *Zeros;                                                                             
        DoW SqlCod = 0;                                                                             
        pos1 +=1;                                                                                   
        %Occur(ResulNomFile) =  pos1;                                                               
        Clear XString;                                                                              
         ResulNomFile.WDesCondi = WNF_Condicion;                                                    
        XString = WNF_Condicion;                                                                    
        PrVlrNumeric();                                                                             
       Monitor;                                                                                     
         Dato1n = %Dec(XString:5:2);                                                                
       On-Error;                                                                                    
         Clear Dato1n;                                                                              
       EndMon;                                                                                      
         ResulNomFile.WCondic  = Dato1n;                                                            
         If Dato1n > WporceCond;                                                                    
            WPosConMx = Pos1;                                                                       
            WporceCond = Dato1n;                                                                    
         Endif;                                                                                     
         Dato2n  = %dec(WNF_Valor:11:2);                                                            
         ResulNomFile.W_valor  = Dato2n;                                                            
                                                                                                    
         Exec Sql                                                                                   
          fetch Cursor6A Into :WNF_Condicion, :WNF_Valor;                                           
        EndDo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor6A;                                                                            
                                                                                                    
      /End-Free                                                                                     
     PPrFileQtemp      E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Retorna Valor numerico para condicion y valor       //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrVlrNumeric     B                                                                            
     DPrVlrNumeric     Pi                                                                           
      /Free                                                                                         
       StringIO = XString;                                                                          
       Clear Vpermitidos;                                                                           
       Clear Caracteres;                                                                            
       Clear String;                                                                                
       Caracteres = '0123456789.,';                                                                 
       XString = *Blanks;                                                                           
       If %Len(%Trim(StringIO)) > *Zeros;                                                           
      /End-Free                                                                                     
     C                   Movea     StringIO      String                                             
      /Free                                                                                         
         For Idx = 1 To %Elem(String) By 1;                                                         
          If %Lookup(String(Idx):Vpermitidos) > *Zeros;                                             
           XString = %Trim(XString)  + %Trim(String(Idx));                                          
          Endif;                                                                                    
         EndFor;                                                                                    
        Endif;                                                                                      
      /End-Free                                                                                     
     PPrVlrNumeric     E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Valida Si el foco es conservar                      //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrValidFoco      B                                                                            
     DPrValidFoco      Pi                                                                           
      /Free                                                                                         
       Wsql = *Blanks;                                                                              
       Wsql = 'Select Count(0) ' +                                                                  
              'From VotreA00/Vtmfcgf '+                                                             
              'Where FcgCia = ' + $Coma + %trim(Icia) + $Coma + ' And ' +                           
              'FcgZon = ' + $Coma + %trim(IZona) + $Coma + ' And ' +                                
              'FcgCmp = ' +  $Coma + %trim(PCamR) + $Coma + ' And ' +                               
              '(FcgFoc = ''K'' Or FcgFoc = ''B'')';                                                 
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor7A From :Wsql;                                                               
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor7A Scroll Cursor For Cursor7A;                                               
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor7A;                                                                             
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor7A Into :WNF_NroFoco;                                                          
                                                                                                    
         XNF_NroFoco = *Zeros;                                                                      
        IF  SqlCod = 0;                                                                             
         XNF_NroFoco = WNF_NroFoco;                                                                 
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor7A;                                                                            
      /End-Free                                                                                     
     PPrValidFoco      E                                                                            
                                                                                                    
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Graba Registro  en archivo Vttpgbf                  //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrGrabar         B                                                                            
     DPrGrabar         Pi                                                                           
      /Free                                                                                         
        W_Reg = ORegion;                                                                            
        W_Cia = ICia;                                                                               
        W_Zona = IZona;                                                                             
        W_Camp = PCamR;                                                                             
        W_foco = Ifoco;                                                                             
                                                                                                    
        chain (W_Cia:W_Reg:W_Zona:WGldsct:WGldcte:W_Camp:W_foco) Vttpgbf;                           
         If Not %Found(Vttpgbf);                                                                    
             Pgbest  = ' ';                                                                         
             Pgbcia  = ICia;                                                                        
             Pgbreg  = ORegion;                                                                     
             Pgbzng  = IZona;                                                                       
             Pgbscg  = WGldsct;                                                                     
             Pgbcdg  = WGldcte;                                                                     
             Pgbcmp  = W_Camp;                                                                      
             Pgbtpf  = Ifoco;                                                                       
             Pgbcon  = WPgbcon;                                                                     
             Pgbvlr  = Wpgbvlr;                                                                     
             Pgbidp  = 'I';                                                                         
             Pgbfec  = %Char(%Date());                                                              
             Pgbhor  = %Char(%Time());                                                              
             Pgbusr  = %Trim(W_USer);                                                               
             write(E) RegPgb;                                                                       
           Else;                                                                                    
            If Pgbidp  = 'I';                                                                       
             Pgbest  = ' ';                                                                         
             Pgbcon  = Wpgbcon;                                                                     
             Pgbvlr  = Wpgbvlr;                                                                     
             Pgbidp  = 'I';                                                                         
             Pgbfec  = %Char(%Date());                                                              
             Pgbhor  = %Char(%Time());                                                              
             Pgbusr  = %Trim(W_USer);                                                               
             Update RegPgb;                                                                         
            EndIf;                                                                                  
        EndIf;                                                                                      
      /End-Free                                                                                     
     PPrGrabar         E                                                                            
