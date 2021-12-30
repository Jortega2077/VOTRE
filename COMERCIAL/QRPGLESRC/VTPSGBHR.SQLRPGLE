     *****************************************************************************                  
      ** Programa     : VTPSGBHR                                                **                  
      ** Descripciòn: : PG Liquida Bono a las Guías para Foco Conseguir         **                  
      ** Proyecto     : Plan Guias                                              **                  
      ** Autor        : Edgar Ivan Maldonado Perez - PERSONALSOFT               **                  
      ** Fecha Creaciòn: 10 de Junio de 2016                                    **                  
      ****************************************************************************                  
     HDftactgrp(*No)                                                                                
     HOption(*NoDebugIo)                                                                            
                                                                                                    
     ** Definiciòn de Archivo de Trabajo                                                            
     ** pagos guias por foco                                                                        
     FVttpgbf   Uf a e           k Disk                                                             
                                                                                                    
      *Definición de variables de trabajo                                                           
     DWoval            s             11  2                                                          
     Dwfile            s             10a   Inz(*Blanks)                                             
     Dwsql             s            200a   Inz(*Blanks)                                             
     DCedula           s              9s 0 Inz(*zeros)                                              
     Dwregio           s             30a   Inz(*Blanks)                                             
     DUser             s             10a   Inz(*User)                                               
     DWcedul           s             10a   Inz(*Blanks)                                             
     DWplan            s             10a   Inz(*Blanks)                                             
     DWsecmain         s              3a   Inz(*Blanks)                                             
     Dnrofoco          s              9s 0 Inz(*zeros)                                              
     Dnrolns           s              9s 0 Inz(*zeros)                                              
     DIcaso            s              7a   Inz(*Blanks)                                             
     DIfecha           s             10a   Inz(*Blanks)                                             
     DIcedula          s             30a   Inz(*Blanks)                                             
     Dwcondicion       s             20a   Inz(*Blanks)                                             
     Dwvalor           s             11s 2 Inz(*zeros)                                              
     DWcampaX          s              4a   Inz(*Blanks)                                             
     DWcampaY          s              4a   Inz(*Blanks)                                             
     DPCamE            s              4a   Inz(*Blanks)                                             
     DPCamR            s              4a   Inz(*Blanks)                                             
     DPOper            s              1a   Inz(*Blanks)                                             
     Dexit             s              1a   Inz(*Blanks)                                             
     DPnume            s              3a   Inz(*Blanks)                                             
     Dvalory           s              1a   Inz(*Blanks)                                             
     DWfoco            s              1a   Inz('G')                                                 
                                                                                                    
     DNumeroValidos    Ds                                                                           
     D                                1    Inz('0')                                                 
     D                                1    Inz('1')                                                 
     D                                1    Inz('2')                                                 
     D                                1    Inz('3')                                                 
     D                                1    Inz('4')                                                 
     D                                1    Inz('5')                                                 
     D                                1    Inz('6')                                                 
     D                                1    Inz('7')                                                 
     D                                1    Inz('8')                                                 
     D                                1    Inz('9')                                                 
      ** Definición de Arreglo de Meses                                                             
     DVctNumeros                      1    Dim(10)                                                  
     D                                     Overlay(NumeroValidos)                                   
     DVctCondiciones                  1    Dim(20)                                                  
     DVctValores                      9  0 Dim(20)                                                  
                                                                                                    
     DUnum             s              5  0 Inz(25)                                                  
     DNum              s              9    Inz(*Blanks)                                             
     DIdx              s              3s 0 Inz(*Zeros)                                              
     DIdx1             s              3s 0 Inz(*Zeros)                                              
     DCondicion        s             20    Inz(*blanks)                                             
     DP                s              2s 0 Inz(*Zeros)                                              
     DWCedreempla      s              9  0  Inz(*Zeros)                                             
     DWCedAux          s              9  0  Inz(*Zeros)                                             
                                                                                                    
     D MemCpy          Pr                  ExtProc('__memcpy')                                      
     D  Target                         *   Value                                                    
     D  Source                         *   Value                                                    
     D  Lenght                       10U 0 Value                                                    
     ** Definicion de Estructuras de Trabajo a Utilizar                                             
     DFilesFile        Ds                                                                           
     DWGldcte                        15s 0 Inz(*Zeros)                                              
     DWGldzon                         3a   Inz(*blanks)                                             
     DWGldsct                         3a   Inz(*blanks)                                             
     DWGldtrr                         1a   Inz(*blanks)                                             
                                                                                                    
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
      * Vlr a Pagar Bonos(Conseguir-Conservar)                                                      
     DPgmconcon        Pr                  Extpgm('VOTREP00/VTMPGBGR')                              
     DXcia                            3                                                             
     DXregion                        30                                                             
     DXzona                           3                                                             
     DXsector                         3                                                             
     DXcampa                          4                                                             
     DXfoco                           1                                                             
     DNomFile                        10                                                             
      * Programa validar region                                                                     
     DPgmregio         Pr                  Extpgm('VOTREP00/VTMGOIGR')                              
     DXcia                            3                                                             
     DXzon                            3                                                             
     DXoreg                          10                                                             
     DXced                           10                                                             
     DXplan                          10                                                             
                                                                                                    
      *Definición de Procedimientos                                                                 
     DPrRangos         Pr                                                                           
     DPrescribir       Pr                                                                           
                                                                                                    
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPCia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //                                                                  //                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //                                                                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /Free                                                                                         
       WcampaY = P_Cam;                                                                             
       WcampaX = P_Cam;                                                                             
       //Invocar programa para calculo de campaña anterior                                          
       PCamE=WcampaX;                                                                               
       PCamR=*Blanks;                                                                               
       POper='-';                                                                                   
       Pnume='001';                                                                                 
       If Wcampax = '2010';                                                                         
        Pnume = '002';                                                                              
       Endif;                                                                                       
       proCampa(P_cia:PCamE:PCamR:POper:Pnume);                                                     
       WcampaX = PcamR;                                                                             
                                                                                                    
        Exec SQL                                                                                    
         Declare C1 Cursor for                                                                      
           Select Distinct                                                                          
             Gldcte, gldzon, Gldsct, Gldtrr                                                         
            From Votrea00/Vtmgldf                                                                   
           Where gldcia = :P_cia And                                                                
                 gldzon = :P_zon And                                                                
           ((Gldest = ' ' And Gldcin <= :WcampaX) Or                                                
           (Gldest = 'D' And Gldcin <= :PcamR And                                                   
           (Gldcdt = :WcampaX Or Gldcdt = :P_Cam Or                                                 
            Gldcdt >= :WcampaX))) And Gldsct <> ' ';                                                
                                                                                                    
        Exec Sql                                                                                    
          Open C1;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch C1 Into :FilesFile;                                                                   
                                                                                                    
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
                                                                                                    
         WCedreempla = *Zeros;                                                                      
         WCedAux = *Zeros;                                                                          
                                                                                                    
         Exec Sql                                                                                   
          Select Rsgcgr Into :WCedreempla                                                           
          From Votrea00/Vtmrsgf                                                                     
          Where Rsgcia = :P_cia And (Rsgtpo = ' ' Or Rsgtpo = 'P') And                              
                Rsgcga = :WGldcte And Rsgzng = :P_zon And Rsgscg = :WGldsct                         
                And :WcampaX BetWeen Rsgcpi And Rsgcpf                                              
          Order By Rsgcpi Asc                                                                       
          Fetch First 1 Row Only;                                                                   
                                                                                                    
          If WCedreempla > *Zeros;                                                                  
           WCedAux = WGldcte;                                                                       
           WGldcte = WCedreempla;                                                                   
          Endif;                                                                                    
                                                                                                    
        Exec SQL                                                                                    
         select count(0) into :nrofoco                                                              
          from votrea00/vtmfcgf                                                                     
          where fcgcia = :p_cia and                                                                 
                fcgzon = :p_zon and                                                                 
                fcgcmp = :wcampax and                                                               
                (fcgfoc ='G' or fcgfoc ='B');                                                       
                                                                                                    
         If nrofoco > *zeros;                                                                       
         Exec SQL                                                                                   
          select count(0) into :nrolns                                                              
           from votrea00/vtwiegf                                                                    
           where iegcia = :P_cia and                                                                
                 iegcdg = :Wgldcte and                                                              
                 iegzng = :wgldzon and                                                              
                 iegscg = :Wgldsct and                                                              
                 iegcmp = :WcampaX and                                                              
                 iegitp = 'E';                                                                      
          // programa de traer la region                                                            
                                                                                                    
            CallP(e) Pgmregio(p_cia:Wgldzon:wregio:Wcedul:wplan);                                   
                                                                                                    
          // Vlr a Pagar Bonos(Conseguir-Conservar)                                                 
          CallP(e) Pgmconcon(p_cia:wregio:Wgldzon:Wgldsct:Wcampax:wfoco:wfile);                     
                                                                                                    
          Wsql = 'Select condicion, valor '                                                         
                 + 'From Qtemp/'+ %Trim(Wfile)                                                      
                 + ' order by rrn('+ %Trim(wfile) + ') desc';                                       
                                                                                                    
          Exec Sql                                                                                  
           Prepare Cur0 From :Wsql;                                                                 
                                                                                                    
          Exec Sql                                                                                  
           Declare Cursor0 Cursor For Cur0;                                                         
                                                                                                    
          Exec Sql                                                                                  
           Open Cursor0;                                                                            
                                                                                                    
          Exec Sql                                                                                  
           Fetch Cursor0 Into :Wcondicion, :Wvalor;                                                 
                                                                                                    
          dow sqlcod = 0;                                                                           
           PRrangos();                                                                              
           if exit='1';                                                                             
            leave;                                                                                  
           endif;                                                                                   
           Exec Sql                                                                                 
            Fetch Cursor0 Into :Wcondicion, :Wvalor;                                                
          enddo;                                                                                    
                                                                                                    
          endif;                                                                                    
                                                                                                    
         If WCedreempla > *Zeros;                                                                   
          WCedreempla = *Zeros;                                                                     
          WGldcte = WCedAux;                                                                        
          WCedAux = *Zeros;                                                                         
         Endif;                                                                                     
                                                                                                    
          Exec Sql                                                                                  
           Fetch C1 Into :FilesFile;                                                                
                                                                                                    
         enddo;                                                                                     
                                                                                                    
       *inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento que verifica rangos de archivo leido               /                         
      //------------------------------------------------------------------/                         
     PPrRangos         B                                                                            
     DPrRangos         Pi                                                                           
                                                                                                    
      /Free                                                                                         
        Clear VctCondiciones;                                                                       
        Clear VctValores;                                                                           
        Clear Idx1;                                                                                 
        // Asigna el String al Vector                                                               
        MemCpy(%Addr(VctCondiciones):                                                               
               %Addr(WCondicion):                                                                   
               %Len(WCondicion));                                                                   
                                                                                                    
        For Idx = 1 To %Elem(VctCondiciones);                                                       
         P = %Lookup(VctCondiciones(Idx):VctNumeros);                                               
         If p <> *Zeros;                                                                            
          Num = %Trim(Num) + %Trim(VctCondiciones(Idx));                                            
         Else;                                                                                      
          If %len(%Trim(Num)) > *Zeros;                                                             
           Idx1 += 1;                                                                               
           VctValores(Idx1) = %Int(%Trim(Num));                                                     
           Num = *Blanks;                                                                           
          Endif;                                                                                    
         Endif;                                                                                     
        EndFor;                                                                                     
        If Idx1 >= 2;                                                                               
         If nrolns >= VctValores(1) And nrolns <= VctValores(2);                                    
          Prescribir();                                                                             
         Endif;                                                                                     
        Else;                                                                                       
         If nrolns >= VctValores(1);                                                                
          Prescribir();                                                                             
         Endif;                                                                                     
        Endif;                                                                                      
      /End-Free                                                                                     
     PPrRangos         E                                                                            
      //------------------------------------------------------------------/                         
      // Procedimiento que graba o actualiza registro                     /                         
      //------------------------------------------------------------------/                         
     PPrescribir       B                                                                            
     DPrescribir       Pi                                                                           
                                                                                                    
      /Free                                                                                         
        exit='0';                                                                                   
        pgbcia = p_cia;                                                                             
        pgbreg = wregio;                                                                            
        pgbzng = p_zon;                                                                             
        pgbscg = wgldsct;                                                                           
        pgbcdg = WGldcte;                                                                           
        pgbcmp = Wcampax;                                                                           
        pgbtpf = 'G';                                                                               
                                                                                                    
        chain (pgbcia:pgbreg:pgbzng:pgbscg:pgbcdg:pgbcmp:pgbtpf) vttpgbf;                           
         if not %found(vttpgbf);                                                                    
          pgbcon = Wcondicion;                                                                      
          pgbvlr = Wvalor;                                                                          
          pgbidp ='I';                                                                              
          pgbfec = %Char(%Date());                                                                  
          pgbhor = %Char(%Time());                                                                  
          pgbusr = user;                                                                            
          write regpgb;                                                                             
          exit ='1';                                                                                
         else;                                                                                      
          if pgbidp='I';                                                                            
           pgbcon = Wcondicion;                                                                     
           pgbvlr = Wvalor;                                                                         
           pgbfec = %Char(%Date());                                                                 
           pgbhor = %Char(%Time());                                                                 
           pgbusr = user;                                                                           
           update regpgb;                                                                           
           exit='1';                                                                                
          endif;                                                                                    
         endif;                                                                                     
                                                                                                    
       /end-Free                                                                                    
     PPrescribir       E                                                                            
