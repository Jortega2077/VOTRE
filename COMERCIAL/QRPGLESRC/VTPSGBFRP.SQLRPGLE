      **********************************************************************                        
      ** Programa     : VTPSGBFR                                          **                        
      ** Descripciòn: : Pago Metas Cumplidas o Metas No Cumplidas Guías   **                        
      **                por Campaña                                       **                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                        
      ** Fecha Creaciòn: 07 de Junio de 2016                              **                        
      **********************************************************************                        
     HOption(*NoDebugIo)                                                                            
     HDFTACTGRP(*NO)                                                                                
                                                                                                    
                                                                                                    
     ** Definiciòn de Archivo de Trabajo                                                            
     FVttdggf   Uf a e           k Disk                                                             
     FVttdggl0  If   e           k Disk    Rename(Regdgg:Rgdgg)                                     
     FVttctgf   Uf a e           k Disk                                                             
                                                                                                    
     ** Definición de Prototipos de Trabajo                                                         
                                                                                                    
     DFields         e Ds                  Extname(Vttdggf:Regdgg)                                  
     D                                     Prefix(Q)                                                
                                                                                                    
     DInfHist          Ds                                                                           
     DXGldcte                        15  0                                                          
     DWlmgcia                         3A                                                            
     DWlmgmed                         4A                                                            
     DWlmgzon                         3A                                                            
     DWlmgsec                         3A                                                            
     DWlmgced                        15  0                                                          
     DWlmgcod                         9  0                                                          
     DWLmgtfa                        11  2                                                          
     DWlmgnir                        10A                                                            
     DWlmgbme                         4A                                                            
     DWlmgnci                         3  0                                                          
     DWlmgcla                        10A                                                            
     DWlmgome                         4A                                                            
     DWlmgtvp                        11  2                                                          
                                                                                                    
                                                                                                    
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
                                                                                                    
     DPgmDatos1        Pr                  Extpgm('VOTREP00/VTPSGB5L')                              
     DPCia                            3a                                                            
     DIZona                           3a                                                            
     DPCamR                           4a                                                            
     DPInd                            1a                                                            
                                                                                                    
      * Modulo Consulta Nro Pedidos Semilla                                                         
     DPgmNroPedSemil   Pr                  Extpgm('VOTREP00/VTMPGB6R')                              
     DIcia                            3a                                                            
     DIRegion                        30a                                                            
     DIZona                           3a                                                            
     DISector                         3a                                                            
     DIcedulaG                        9  0                                                          
     DOnro                            4  0                                                          
     DIndW                            1a                                                            
                                                                                                    
      * Programa validar region                                                                     
     DPgmregio         Pr                  Extpgm('VOTREP00/VTMGOIGR')                              
     DIcia                            3a                                                            
     DIzona                           3a                                                            
     DORegion                        30a                                                            
     DOced                           10A                                                            
     DOplan                          10a                                                            
                                                                                                    
      * Programa Recupera el Codigo de Meta                                                         
     DPgmCodMeta       Pr                  Extpgm('VOTREP00/VTMPGBER')                              
     DIcia                            3a                                                            
     DIMeta                           1a                                                            
     DOCodMeta                        4  0                                                          
                                                                                                    
      * Programa Recupera el Codigo de Meta                                                         
     DPgmCodClasif     Pr                  Extpgm('VOTREP00/VTMPGBFR')                              
     DIcia                            3a                                                            
     DICdMeta                         4  0                                                          
     DITpCla                          4  0                                                          
     DOcdCla                          4  0                                                          
                                                                                                    
      * Programa Consulta Nivel alas Cierre                                                         
     DPgmNivlAlasCier  Pr                  Extpgm('VOTREP00/VTMPGBDR')                              
     DIcia                            3a                                                            
     DIRegion                        30a                                                            
     DIZona                           3a                                                            
     DISector                         3a                                                            
     DIcamPa                          4a                                                            
     DIcedulaG                        9  0                                                          
     DOcdAlas                         4  0                                                          
                                                                                                    
      * Programa Consulta Nivel alas Cierre                                                         
     DPgmVlrPgrCom     Pr                  Extpgm('VOTREP00/VTMPGBCR')                              
     DIcia                            3a                                                            
     DIRegion                        30a                                                            
     DIZona                           3a                                                            
     DISector                         3a                                                            
     DIcampa                          4a                                                            
     DICodMeta                        4  0                                                          
     DIcdClas                         4  0                                                          
     DIcdAlas                         4  0                                                          
     DOValor                         11  2                                                          
                                                                                                    
      *Definición de variables de trabajo                                                           
     DPesw             S              1a   Inz(*Blanks)                                             
     DPCia             S              3a   Inz(*Blanks)                                             
     DPCamE            S              4a   Inz(*Blanks)                                             
     DPCamR            S              4a   Inz(*Blanks)                                             
     DPCamRSE          S              4a   Inz(*Blanks)                                             
     DPOper            S              1a   Inz(*Blanks)                                             
     DPnume            S              3a   Inz(*Blanks)                                             
     DWGldcte          S              9  0 Inz(*Zeros)                                              
     DWGldzon          S              3a   Inz(*Blanks)                                             
     DWGldsct          S              3a   Inz(*Blanks)                                             
     DWGldtrr          S              1a   Inz(*Blanks)                                             
     DWGldcin          S              4a   Inz(*Blanks)                                             
     DWGldest          S              1a   Inz(*Blanks)                                             
     DWGldcdt          S              4a   Inz(*Blanks)                                             
     DWsql             S           1000a   Inz(*Blanks)                                             
     D$COMA            C                   ''''                                                     
     DWvalor           S             15  0 Inz(*Zeros)                                              
     DXEdenro          S              9  0 Inz(*Zeros)                                              
     DWPdosMt          S              9  0 Inz(*Zeros)                                              
     DWPdosMetas       S              9  0 Inz(*Zeros)                                              
     DORegion          S             30A   Inz(*Blanks)                                             
     DOCed             S             10A   Inz(*Blanks)                                             
     DOplan            S             10a   Inz(*Blanks)                                             
     DOnro             S              4  0 Inz(*Zeros)                                              
     DIMeta            S              1a   Inz(*Blanks)                                             
     DOCdMeta          S              4  0 Inz(*Zeros)                                              
     DWPDSEncima       S              4  0 Inz(*Zeros)                                              
     DWSemilla         S              4  0 Inz(*Zeros)                                              
     DITpCla           S              4  0 Inz(*Zeros)                                              
     DIcdmeta          S              4  0 Inz(*Zeros)                                              
     DOcdCla           S              4  0 Inz(*Zeros)                                              
     DOcdAlas          S              4  0 Inz(*Zeros)                                              
     DOValor           S             11  2 Inz(*Zeros)                                              
     DIcdClas          S              4  0 Inz(*Zeros)                                              
     DIcdAlas          S              4  0 Inz(*Zeros)                                              
     DWVAR             S              5  0 Inz(*Zeros)                                              
     DWVARW            S              5  0 Inz(*Zeros)                                              
     DWVAREN           S              5  0 Inz(*Zeros)                                              
     DW_USER           s             10a   Inz(*User)                                               
     DIcaso            s              7a   Inz(*Blanks)                                             
     DIfecha           s             10a   Inz(*Blanks)                                             
     DIcedula          s             30a   Inz(*Blanks)                                             
     DOrespuestaP      s              1a   Inz(*Blanks)                                             
     DW_Efetividad     s              1a   Inz(*Blanks)                                             
     DW_Orden          s              9  0 Inz(*Zeros)                                              
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DIndW             s              1a   Inz(*Blanks)                                             
     DXCedAux          s              9  0 Inz(*Zeros)                                              
     DPind             s              1a   Inz(*Blanks)                                             
     DPCamEa           s              4a   Inz(*Blanks)                                             
     DPCamRa           S              4a   Inz(*Blanks)                                             
     DPOpera           S              1a   Inz(*Blanks)                                             
     DPnumea           S              3a   Inz(*Blanks)                                             
     DKcam             s              4a                                                            
     DTfecha           s             10a   Inz(*Blanks)                                             
     DGldsct           s              1a   Inz(*Blanks)                                             
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
                                                                                                    
     ** Consulta efectividad de Pedido                                                              
     DPgmEfectividad   Pr                  Extpgm('VOTREP00/VTPEFE0R')                              
     DXCia                            3                                                             
     DXCed                            9  0                                                          
     DXOrd                            9  0                                                          
     DXEfe                            1a                                                            
                                                                                                    
     DPrPedidoFacturado...                                                                          
     D                 Pr             9  0                                                          
     DPrVttctgf        Pr                                                                           
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
         // pgm retorna Región                                                                      
            CallP(e) Pgmregio(ICia:IZona:ORegion:Oced:Oplan);                                       
        //Resta de la  Campaña Cierre de Zona Menos 5 Campañas                                      
           PCamE = ICamPa;                                                                          
           PCamR = *Blanks;                                                                         
           POper = '-';                                                                             
           Pnume = '1';                                                                             
           If ICampa = '2010';                                                                      
            Pnume = '002';                                                                          
           Endif;                                                                                   
           callp(E) ProCampa(Icia:PCamE:PCamR:POper:Pnume);                                         
       Wsql = *Blanks;                                                                              
       Wsql = 'SELECT Gldcte, GLDZON, GLDSCT, GLDTRR, GLDCIN, GLDEST, ' +                           
              'Gldcdt ' +                                                                           
              'FROM VotreA00/Vtmgldf '+                                                             
              'WHERE GLDCIA = ' + $Coma + %trim(Icia) + $Coma + ' And ' +                           
              'GLDZON = ' + $Coma + %trim(IZona) + $Coma + ' And ' +                                
              '((GLDEST = '' ''' + ' And Gldcin <= ' + '''' +                                       
              PCamR + '''' + ') Or (GLDEST = ''D'' And Gldcin <= ' +                                
              '''' + PCamR + '''' + ' And (' +                                                      
              ' Gldcdt = ' +  $Coma + %trim(ICampa) + $Coma + ' Or ' +                              
              ' Gldcdt = ' +  $Coma + %trim(PCamR) + $Coma + ' Or ' +                               
              ' Gldcdt >= ' +  $Coma + %trim(PCamR) + $Coma + ')))' +                               
              ' And Gldsct <> ' + '''' + ' ' + '''' +                                               
              ' And Gldcte = 995263995';                                                            
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor1 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor1 Scroll Cursor For Cursor1;                                                 
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor1;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor1 Into :WGldcte, :WGldzon, :WGldsct, :WGldtrr, :WGldcin,                       
                            :WGldest, :WGldcdt;                                                     
                                                                                                    
        Dow sqlcod=*Zeros;                                                                          
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
           If KCam = '2008' And ICampa = '2010';                                                    
            Pnume = '002';                                                                          
           Endif;                                                                                   
           callp(E) ProCampa(Icia:PCamEa:PCamRa:POpera:Pnumea);                                     
           Kcam = PCamRa;                                                                           
          Endif;                                                                                    
           //If PCamR >= Kcam;                                                                      
           If ICamPa>= Kcam;                                                                        
          If OrespuestaP = 'S';                                                                     
                                                                                                    
          // A la Variable Wvalor se le Asigna el Valor del Ppto Archivo de Sandra                  
          // Meta a Consultar para saber si cumplio o no                                            
          Wvalor = *Zeros;                                                                          
          Imeta = *Blanks;                                                                          
          Exec Sql                                                                                  
           Select Pedpts Into :Wvalor                                                               
           From Votrea00/Vtbptofs                                                                   
           Where Ciapts = :Icia And                                                                 
                 Campts = :PCamR And                                                                
                 Zonpts = :WGldzon And                                                              
                 Secpts = :WGldsct                                                                  
           Fetch First 1 Row Only;                                                                  
                                                                                                    
          WPdosMetas = *Zeros;                                                                      
          // Clasificación de Metas en con la Campaña Anterior a                                    
          // la Campaña de Cierre se Cierra Campaña 1705, el                                        
          // Sistema Paga con la Clasifica de Campaña 1704                                          
          LeerClasMetas();                                                                          
          If WPdosMetas = *Zeros;                                                                   
           // Se Invoca Programas para Llenar el Archivo Vtglmgf                                    
           // Programas de Andres Parra                                                             
            Pind = 'D';                                                                             
            PgmDatos1(Icia:WGldzon:PCamR:Pind);                                                     
            Pind = 'A';                                                                             
            PgmDatos1(Icia:WGldzon:PCamR:Pind);                                                     
            WPdosMetas = *Zeros;                                                                    
            LeerClasMetas();                                                                        
          Else;                                                                                     
           If WGldcin = PCamR;                                                                      
            Pind = 'D';                                                                             
            PgmDatos1(Icia:WGldzon:PCamR:Pind);                                                     
            Pind = 'A';                                                                             
            PgmDatos1(Icia:WGldzon:PCamR:Pind);                                                     
            WPdosMetas = *Zeros;                                                                    
            LeerClasMetas();                                                                        
           Endif;                                                                                   
          Endif;                                                                                    
          LeerClasAlas();                                                                           
          If (WPdosMetas >= Wvalor And Wvalor > *Zeros);                                            
          Clear IndW;                                                                               
          If WGldest = 'D';                                                                         
           PCamE = ICamPa;                                                                          
           PCamRSE = *Blanks;                                                                       
           POper = '-';                                                                             
           Pnume = '1';                                                                             
           If ICampa = '2010';                                                                      
            Pnume = '002';                                                                          
           Endif;                                                                                   
           callp(E) ProCampa(Icia:PCamE:PCamRSE:POper:Pnume);                                       
           If WGldcdt >= PCamRSE;                                                                   
            IndW = 'C';                                                                             
           Endif;                                                                                   
          Endif;                                                                                    
          Clear Onro;                                                                               
        callp(E) PgmNroPedSemil(Icia:ORegion:WGldzon:WGldsct:WGldcte:Onro:IndW);                    
             Wsemilla = Onro;                                                                       
             WPdsEncima = XEdenro - WSemilla;                                                       
                                                                                                    
             Exec Sql                                                                               
              Insert Into Jortega/Semilla(Cia, CampaÑa, Zona, Sector,                               
                                         Cedula, Semilla, Encima)                                   
              Values(:Icia, :PCamR, :Wgldzon, :Wgldsct, :Wgldcte,                                   
                     :Wsemilla, :WPdsEncima);                                                       
                                                                                                    
             If WPdsEncima <= *Zeros;                                                               
              Wsemilla = XEdenro;                                                                   
              WPdsEncima = *Zeros;                                                                  
             Endif;                                                                                 
             IMeta = 'S';                                                                           
             PrVttctgf();                                                                           
            Callp(E) pgmCodMeta(ICia:IMeta:Ocdmeta);                                                
             ITpCla = 0001;                                                                         
             IcdMeta = OcdMeta;                                                                     
            Callp(E) pgmCodClasif(ICia:IcdMeta:ITpCla:OcdCla);                                      
             IZona = WGldzon;                                                                       
            callp(E) PgmNivlAlasCier(Icia:ORegion:IZona:WGldsct:PcamR:WGldcte                       
                                     :OcdAlas);                                                     
             IcdClas = OcdCla;                                                                      
             IcdAlas = OcdAlas;                                                                     
            Callp(E) PgmVlrPgrCom(Icia:ORegion:IZona:WGldsct:PcamR:ICdMeta                          
                          :IcdClas:IcdAlas:OValor);                                                 
             WVar=0;                                                                                
             WVaren=0;                                                                              
            PrleeGuiaCam();                                                                         
          EndIf;                                                                                    
          If (WPdosMetas < Wvalor) Or (Wvalor <= *Zeros);                                           
          IndW = *Blanks;                                                                           
          If WGldest = 'D';                                                                         
           IndW = 'C';                                                                              
          Endif;                                                                                    
        callp(E) PgmNroPedSemil(Icia:ORegion:WGldzon:WGldsct:WGldcte:Onro:IndW);                    
             Wsemilla = Onro;                                                                       
             WPdsEncima = XEdenro - WSemilla;                                                       
                                                                                                    
             Exec Sql                                                                               
              Insert Into Jortega/Semilla(Cia, CampaÑa, Zona, Sector,                               
                                         Cedula, Semilla, Encima)                                   
              Values(:Icia, :PCamR, :Wgldzon, :Wgldsct, :Wgldcte,                                   
                     :Wsemilla, :WPdsEncima);                                                       
                                                                                                    
             If WPdsEncima <= *Zeros;                                                               
              Wsemilla = XEdenro;                                                                   
              WPdsEncima = *Zeros;                                                                  
             Else;                                                                                  
              WPdsEncima = XEdenro - WSemilla;                                                      
             Endif;                                                                                 
             IMeta = 'N';                                                                           
             PrVttctgf();                                                                           
            Callp(E) pgmCodMeta(ICia:IMeta:Ocdmeta);                                                
             ITpCla = 0001;                                                                         
             IcdMeta = OcdMeta;                                                                     
            Callp(E) pgmCodClasif(ICia:IcdMeta:ITpCla:OcdCla);                                      
             IZona = WGldzon;                                                                       
            callp(E) PgmNivlAlasCier(Icia:ORegion:IZona:WGldsct:PcamR:WGldcte                       
                                     :OcdAlas);                                                     
             IcdClas = OcdCla;                                                                      
             IcdAlas = OcdAlas;                                                                     
            Callp(E) PgmVlrPgrCom(Icia:ORegion:IZona:WGldsct:PcamR:ICdMeta                          
                          :IcdClas:IcdAlas:OValor);                                                 
             WVar=0;                                                                                
             WVaren=0;                                                                              
            PrleeGuiaCam();                                                                         
                                                                                                    
          EndIf;                                                                                    
          EndIf;                                                                                    
          EndIf;                                                                                    
         Exec Sql                                                                                   
         fetch Cursor1 Into :WGldcte, :WGldzon, :WGldsct, :WGldtrr, :WGldcin,                       
                            :WGldest, :WGldcdt;                                                     
        EndDo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor1;                                                                             
                                                                                                    
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Leer Clasificación de Alas                          //                        
      //////////////////////////////////////////////////////////////////////                        
     PLeerClasAlas     B                                                                            
     DLeerClasAlas     Pi                                                                           
      /Free                                                                                         
       Wsql = *Blanks;                                                                              
       Wsql = 'Select Count(0) ' +                                                                  
              'From Votrea00/Vtglmgf ' +                                                            
              'Where Lmgcia = ' + $Coma + %trim(Icia) + $Coma + ' And ' +                           
              'Lmgmed = ' + $Coma + %trim(PCamR) + $Coma + ' And ' +                                
              'Lmgsec = ' + $Coma + %trim(WGldsct) + $Coma +  ' And ' +                             
              'Lmgzon = ' + $Coma + %trim(Izona) + $Coma +  ' And ' +                               
              'Lmgtpe <> 0 And Lmgpef = ''X''';                                                     
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor2 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor2 Scroll Cursor For Cursor2;                                                 
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor2;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor2 Into :WPdosMt;                                                               
                                                                                                    
           XEdenro = 0;                                                                             
        If sqlcod=0;                                                                                
           XEdenro = WPdosMt;                                                                       
           EndIf;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         Close cursor2;                                                                             
                                                                                                    
      /End-Free                                                                                     
     PLeerClasAlas     E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Leer Clasificación de Metas                         //                        
      //////////////////////////////////////////////////////////////////////                        
     PLeerClasMetas    B                                                                            
     DLeerClasMetas    Pi                                                                           
      /Free                                                                                         
       WPdosMetas = *Zeros;                                                                         
       Wsql = *Blanks;                                                                              
       Wsql = 'Select Count(0) ' +                                                                  
              'From Votrea00/Vtglmgf ' +                                                            
              'Where Lmgcia = ' + $Coma + %trim(Icia) + $Coma + ' And ' +                           
              'Lmgmed = ' + $Coma + %trim(PCamR) + $Coma + ' And ' +                                
              'Lmgsec = ' + $Coma + %trim(WGldsct) + $Coma +  ' And ' +                             
              'Lmgzon = ' + $Coma + %trim(Izona) + $Coma +  ' And ' +                               
              'Lmgtpe <> 0';                                                                        
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor2m From :Wsql;                                                               
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor2m Scroll Cursor For Cursor2m;                                               
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor2m;                                                                             
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor2m Into :WPdosMetas;                                                           
                                                                                                    
       If sqlcod = 100 Or sqlCod < *Zeros;                                                          
        WPdosMetas = *Zeros;                                                                        
       EndIf;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close cursor2m;                                                                            
                                                                                                    
      /End-Free                                                                                     
     PLeerClasMetas    E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Lectura al archivo de Guías y Pedidos por  Campaña  //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrleeGuiaCam     B                                                                            
     DPrleeGuiaCam     Pi                                                                           
      *Definición de variables de trabajo                                                           
     DGldcte           s              9  0 Inz(*Zeros)                                              
     DLmgcod           s              9  0 Inz(*Zeros)                                              
     DWcod             s              4  0 Inz(*Zeros)                                              
      /Free                                                                                         
       Gldsct = *Blanks;                                                                            
       Wsql = *Blanks;                                                                              
       Wsql = 'Select Distinct Gldcte, Lmgcod, Gldsct ' +                                           
              'From Votrea00/Vtglmgf Inner Join Votrea00/Vtmgldf On ' +                             
              'Lmgcia=Gldcia And Lmgzon=Gldzon And Lmgsec = Gldsct ' +                              
              'Where Lmgcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'Lmgmed = ' + $Coma + %trim(ICamPa) + $Coma + ' And ' +                               
              'Gldzon = ' + $Coma + %trim(IZona) + $Coma + ' And ' +                                
              'Gldcte = ' + %trim(%char(WGldcte)) + ' And (' +                                      
              'Gldest = '' '' Or (Gldest = ''D'' And ' +                                            
              '(Gldcdt = ' + $Coma + %trim(ICampa) + $Coma + ' Or ' +                               
              'Gldcdt = ' + $Coma + %trim(PCamR) + $Coma + ' Or ' +                                 
              'Gldcdt >= ' + $Coma + %trim(PCamR) + $Coma + ')))' +                                 
              ' And ' +                                                                             
              'Lmgsec = ' + '''' + %Trim(WGldsct) + '''' +                                          
              'Order By Gldsct ';                                                                   
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor4 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor4 Cursor For Cursor4;                                                        
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor4;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor4 Into :Gldcte, :Lmgcod, :Gldsct;                                              
                                                                                                    
       WVAR = 0;                                                                                    
       Wvaren = 1;                                                                                  
       WvarW = *Zeros;                                                                              
       Wcod = *Zeros;                                                                               
       Exec Sql                                                                                     
        Select Dggcgc Into :Wcod                                                                    
        From Votrea00/Vttdggf                                                                       
        Where Dggcia = :Icia And                                                                    
              Dggzng = :Izona And                                                                   
              Dggscg = :Gldsct And                                                                  
              Dggcdg = :Gldcte And                                                                  
              Dggcmp = :PCamR                                                                       
        Order By Rrn(Vttdggf) Asc                                                                   
        Fetch First 1 Row Only;                                                                     
                                                                                                    
       Exec Sql                                                                                     
        Select Count(0) Into :WvarW                                                                 
        From Votrea00/Vttdggf                                                                       
        Where Dggcia = :Icia And                                                                    
              Dggzng = :Izona And                                                                   
              Dggscg = :Gldsct And                                                                  
              Dggcdg = :Gldcte And                                                                  
              Dggcmp = :PCamR And                                                                   
              Dggcgc = :Wcod;                                                                       
       If WvarW > *Zeros;                                                                           
        Wvar = WvarW;                                                                               
       Endif;                                                                                       
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
       Wsql = *Blanks;                                                                              
       Wsql = 'Select Distinct ' +                                                                  
              'Gldcte, Lmgcia, Lmgmed, Lmgzon, Lmgsec, Lmgced, Lmgcod, ' +                          
              'Lmgtfa, Lmgnir, Lmgbme, Lmgnci, Lmgcla, Lmgome, Lmgtvp ' +                           
              'From Votrea00/Vtglmgf Inner Join Votrea00/Vtmgldf On ' +                             
              'Lmgcia=Gldcia ' +                                                                    
              'Where Lmgcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'Lmgmed = ' + $Coma + %trim(PCamR) + $Coma + ' ' +                                    
              'And Lmgpef = ''X''' + ' And Gldsct = ' + '''' + Gldsct +                             
              '''' + ' And Lmgcod = ' + %Trim(%Editc(Lmgcod:'Z')) +                                 
              ' And Gldcte = ' + %Trim(%Editc(Gldcte:'Z')) +                                        
              ' And Lmgsec = Gldsct';                                                               
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor4q From :Wsql;                                                               
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor4q Cursor For Cursor4q;                                                      
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor4q;                                                                             
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor4q Into :InfHist;                                                              
                                                                                                    
        Dow SqlCod = 0;                                                                             
         If Wlmgcod = 42200533;                                                                     
          Dsply 'pp';                                                                               
         Endif;                                                                                     
         W_Orden = PrPedidoFacturado();                                                             
         If Pesw = *Blanks;                                                                         
          W_Efetividad = *Blanks;                                                                   
          XCedAux = *Zeros;                                                                         
          XCedAux = Wlmgcod;                                                                        
          PgmEfectividad(Wlmgcia:XCedAux:W_Orden:W_Efetividad);                                     
         Else;                                                                                      
          W_Efetividad = 'S';                                                                       
         Endif;                                                                                     
         If W_Efetividad = 'S';                                                                     
          WVAR += 1;                                                                                
          If Wvar <= Wsemilla;                                                                      
           PrGrabar();                                                                              
          Endif;                                                                                    
                                                                                                    
          If WPDSEncima > *Zeros And Wvar > Wsemilla;                                               
           If Wvaren <= WPDSEncima;                                                                 
            IcdMeta = OcdMeta;                                                                      
            ITpCla =  0002;                                                                         
            Callp(E) pgmCodClasif(ICia:IcdMeta:ITpCla:OcdCla);                                      
            IcdClas = OcdCla;                                                                       
            IcdAlas = OcdAlas;                                                                      
            Callp(E) PgmVlrPgrCom(Icia:ORegion:IZona:WLmgsec:WLmgmed:ICdMeta                        
                           :IcdClas:IcdAlas:OValor);                                                
            WVaren += 1;                                                                            
            PrGrabar();                                                                             
           EndIf;                                                                                   
          EndIf;                                                                                    
         Endif;                                                                                     
         Exec Sql                                                                                   
          Fetch Cursor4q Into :InfHist;                                                             
        EndDo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor4q;                                                                            
                                                                                                    
         Exec Sql                                                                                   
          Fetch Cursor4 Into :Gldcte, :Lmgcod, :Gldsct;                                             
        EndDo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor4;                                                                             
      /End-Free                                                                                     
     PPrleeGuiaCam     E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Graba Registro  en archivo VTTDGGF                  //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrGrabar         B                                                                            
     DPrGrabar         Pi                                                                           
      *Definición de variables de trabajo                                                           
     DWCedreempla      s              9  0  Inz(*Zeros)                                             
     DWCedAux          s              9  0  Inz(*Zeros)                                             
      /Free                                                                                         
                                                                                                    
       If OValor <> *Zeros;                                                                         
                                                                                                    
       Exec Sql                                                                                     
        Select Rsgcgr Into :WCedreempla                                                             
        From Votrea00/Vtmrsgf                                                                       
        Where Rsgcia = :Icia And (Rsgtpo = ' ' Or Rsgtpo = 'P') And                                 
              Rsgcga = :XGldcte And Rsgzng = :Izona And Rsgscg = :WGldsct                           
              And :PCamR BetWeen Rsgcpi And Rsgcpf                                                  
        Order By Rsgcpi Asc                                                                         
        Fetch First 1 Row Only;                                                                     
                                                                                                    
       If WCedreempla > *Zeros;                                                                     
        WCedAux = XGldcte;                                                                          
        XGldcte = WCedreempla;                                                                      
       Endif;                                                                                       
                                                                                                    
        chain (ICia:ORegion:IZona:WGldsct:XGldcte:PCamR:WLmgzon                                     
              :WLmgsec:WLmgcod) Vttdggl0;                                                           
         If Not %Found(Vttdggl0);                                                                   
        chain (ICia:ORegion:IZona:WGldsct:XGldcte:PCamR:WLmgzon                                     
              :WLmgsec:WLmgcod:OcdMeta:OcdCla:OcdAlas) Vttdggf;                                     
         If Not %Found(Vttdggf);                                                                    
               DggCia  = ICia;                                                                      
               DggReg  = ORegion;                                                                   
               DggZng  = IZona;                                                                     
               DggSCG  = WGldsct;                                                                   
               Dggcdg  = XGldcte;                                                                   
               Dggcmp  = PCamR;                                                                     
               DggZnc  = WLmgzon;                                                                   
               If WLmgsec <> Gldsct;                                                                
                Dggscc  = Gldsct;                                                                   
               Else;                                                                                
                Dggscc  = WLmgsec;                                                                  
               Endif;                                                                               
               Dggcdc  = WLmgcod;                                                                   
               DggCgm  = OcdMeta;                                                                   
               DggCgc  = OcdCla;                                                                    
               DggCga  = OcdAlas;                                                                   
               DggVlr  = OValor;                                                                    
               DggIDP  = 'I';                                                                       
               DggFec  = %Char(%Date());                                                            
               DggHor  = %Char(%Time());                                                            
               DggUsr  = %Trim(W_USer);                                                             
               Write Regdgg;                                                                        
         Else;                                                                                      
               DggVlr  = OValor;                                                                    
               DggIDP  = 'I';                                                                       
               DggFec  = %Char(%Date());                                                            
               DggHor  = %Char(%Time());                                                            
               DggUsr  = %Trim(W_USer);                                                             
               Update Regdgg;                                                                       
         EndIf;                                                                                     
       EndIf;                                                                                       
                                                                                                    
       If WCedreempla > *Zeros;                                                                     
        WCedreempla = *Zeros;                                                                       
        XGldcte = WCedAux;                                                                          
        WCedAux = *Zeros;                                                                           
       Endif;                                                                                       
                                                                                                    
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrGrabar         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si el Pedido esta almenos Facturado   //                       
       //------------------------------------------------------------------//                       
     PPrPedidoFacturado...                                                                          
     P                 B                                                                            
     DPrPedidoFacturado...                                                                          
     D                 Pi             9  0                                                          
      ** Definicion de Variables a Utilizar                                                         
     DQnro             s              9  0 Inz(*Zeros)                                              
      /Free                                                                                         
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select OhordÑ ' +                                                                  
                'From Opf' + Icia + '/Porhdr ' +                                                    
                'Where OhcstÑ = ' + %Trim(%Editc(Wlmgcod:'Z')) + ' And ' +                          
                'Ohmeda = ' + '''' + PcamR + '''' + ' And ' +                                       
                'Ohcmfl = ''N''' + ' And Ohrcst = ''S''' + ' ' +                                    
                'Group By Trim(Ohmeda), OhordÑ, Rrn(Porhdr) ' +                                     
                'Order By Trim(Ohmeda), OhordÑ, Rrn(Porhdr) Asc ' +                                 
                'Fetch First 1 Row Only';                                                           
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur0 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor0 Cursor For Cur0;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor0;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor0 Into :Qnro;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor0;                                                                              
                                                                                                    
       Pesw = *Blanks;                                                                              
       If Qnro = *Zeros;                                                                            
        Qnro = PrInsuficiencia();                                                                   
        If Qnro <> *Zeros;                                                                          
         Pesw = 'S';                                                                                
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       Return Qnro;                                                                                 
      /End-Free                                                                                     
     PPrPedidoFacturado...                                                                          
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si el Pedido esta Cancelado por Insu  //                       
       // ficiencia de Mercancia                                           //                       
       //------------------------------------------------------------------//                       
     PPrInsuficiencia...                                                                            
     P                 B                                                                            
     DPrInsuficiencia...                                                                            
     D                 Pi             9s 0                                                          
      ** Definicion de Variables a Utilizar                                                         
     DQnro             s              9s 0 Inz(*Zeros)                                              
     DQnroX            s              9s 0 Inz(*Zeros)                                              
     DQnum             s              9  0 Inz(*Zeros)                                              
      /Free                                                                                         
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select OhordÑ ' +                                                                  
                'From Opf' + Icia + '/Porhdr ' +                                                    
                'Where OhcstÑ = ' + %Trim(%Editc(Wlmgcod:'Z')) + ' And ' +                          
                'Ohmeda = ' + '''' + PcamR + '''' + ' And ' +                                       
                'Ohcmfl = ''N''' + ' And Ohrcst = ''C'''+                                           
                ' Group By Trim(Ohmeda), OhordÑ, Rrn(Porhdr) ' +                                    
                'Order By Trim(Ohmeda), OhordÑ, Rrn(Porhdr) Asc ' +                                 
                'Fetch First 1 Row Only';                                                           
                                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur1a From :SqlStr;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1a Cursor For Cur1a;                                                          
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1a;                                                                              
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1a Into :QnroX;                                                                 
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
                                                                                                    
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Count(Distinct(OdordÑ)) ' +                                                 
                'From Opf' + Icia + '/Pordtl ' +                                                    
                'Where Odscus = ' + %Trim(%Editc(Wlmgcod:'Z')) +                                    
                ' And OdordÑ = ' + %Trim(%Editc(QnroX:'Z')) +                                       
            ' And (Odlccd = ''INS''' + ' Or Odlccd = ''BAC''' + ' Or ' +                            
            'Odlccd = ''CCD''' + ')';                                                               
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur1p From :SqlStr;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1p Cursor For Cur1p;                                                          
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1p;                                                                              
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1p Into :Qnum;                                                                  
                                                                                                    
       If Qnum > *Zeros;                                                                            
        Qnro = QnroX;                                                                               
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor1p;                                                                             
                                                                                                    
        Exec Sql                                                                                    
         Fetch Cursor1a Into :QnroX;                                                                
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor1a;                                                                             
                                                                                                    
       Return Qnro;                                                                                 
      /End-Free                                                                                     
     PPrInsuficiencia...                                                                            
     P                 E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Graba Registro en archivo Vttctgf                   //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrVttctgf        B                                                                            
     DPrVttctgf        Pi                                                                           
      *Definición de variables de trabajo                                                           
     DWCedreempla      s              9  0  Inz(*Zeros)                                             
      /Free                                                                                         
       Chain (ICia:PCamR:WGldcte:IZona:WGldsct) Vttctgf;                                            
       If Not %Found(Vttctgf);                                                                      
        Ctgcia = ICia;                                                                              
        Ctgcam = PCamR;                                                                             
        Ctgcdg = WGldcte;                                                                           
        Ctgzon = IZona;                                                                             
        Ctgsec = WGldsct;                                                                           
        Ctgptc = Wvalor;                                                                            
        Ctgptg = WPdosMetas;                                                                        
        Ctgidc = 'No';                                                                              
        If IMeta = 'S';                                                                             
         Ctgidc = 'Si';                                                                             
        Endif;                                                                                      
        Write Regctg;                                                                               
       Else;                                                                                        
        Ctgptc = Wvalor;                                                                            
        Ctgptg = WPdosMetas;                                                                        
        Ctgidc = 'No';                                                                              
        If IMeta = 'S';                                                                             
         Ctgidc = 'Si';                                                                             
        Endif;                                                                                      
        Update Regctg;                                                                              
       Endif;                                                                                       
                                                                                                    
      /End-Free                                                                                     
     PPrVttctgf        E                                                                            
