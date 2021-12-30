***************************************************************************                         
     ** Programa      : VTPSGB1R                                          *                         
     ** Descripción:  : Política Inactivar Guías por RUT                  *                         
     ** Proyecto      : Nuevo Plan de Guías                               *                         
     ** Autor         : (JOrtega)                                         *                         
     ** Fecha Creación: 14 de Marzo de 2016                               *                         
     **********************************************************************                         
                                                                                                    
      ** Definición de Directivas de Compilación                                                    
       Ctl-Opt Dftactgrp(*no) Bnddir('QC2LE');                                                      
                                                                                                    
      ** Definición de Archivos de Trabajo                                                          
       Dcl-f Vttpprf Disk Usage(*Input:*Output) Keyed;                                              
                                                                                                    
      ** Definición de Variables de Trabajo                                                         
       Dcl-s Wzona Char(3) Inz(*Blanks);                                                            
       Dcl-s Wnombre Char(50) Inz(*Blanks);                                                         
       Dcl-s Cedula Packed(15:0) Inz(*Zeros);                                                       
       Dcl-s CmpIng Char(4) Inz(*Blanks);                                                           
       Dcl-s Icia Char(3) Inz(*Blanks);                                                             
       Dcl-s Izona Char(3) Inz(*Blanks);                                                            
       Dcl-s Icaso Char(7) Inz(*Blanks);                                                            
       Dcl-s Icedula Char(30) Inz(*Blanks);                                                         
       Dcl-s Ifecha Char(10) Inz(*Blanks);                                                          
       Dcl-s Orespuesta Char(1) Inz(*Blanks);                                                       
       Dcl-s PCamE Char(4) Inz(*Blanks);                                                            
       Dcl-s PCamR Char(4) Inz(*Blanks);                                                            
       Dcl-s POper Char(1) Inz(*Blanks);                                                            
       Dcl-s Pnume Char(3) Inz(*Blanks);                                                            
       Dcl-s IcedLar Char(30) Inz(*Blanks);                                                         
       Dcl-s IcedCor Char(9) Inz(*Blanks);                                                          
       Dcl-s Kcia Char(10)Inz(*Blanks);                                                             
       Dcl-s Kres Char(2) Inz(*Blanks);                                                             
       Dcl-s KciaR Char(10) Inz(*Blanks);                                                           
       Dcl-s Wsector Char(1) Inz(*Blanks);                                                          
       Dcl-s Wterr Char(1) Inz(*Blanks);                                                            
       Dcl-s Wuser Char(10) Inz(*Blanks);                                                           
       Dcl-s Wlider Char(1000) Inz(*Blanks);                                                        
       Dcl-s Wregional Char(1000) Inz(*Blanks);                                                     
       Dcl-s Wcomercial Char(1000) Inz(*Blanks);                                                    
       Dcl-s w_Email Char(200) Inz(*Blanks);                                                        
       Dcl-s Respuesta Char(8) Inz(*Blanks);                                                        
       Dcl-s Region Char(10) Inz(*Blanks);                                                          
       Dcl-s CamRutV Char(4) Inz(*Blanks);                                                          
       Dcl-s SpgciaQ Char(3) Inz(*Blanks);                                                          
       Dcl-s SpgzonQ Char(3) Inz(*Blanks);                                                          
       Dcl-s SpgcedQ Packed(9:0) Inz(*Zeros);                                                       
       Dcl-s SpgsecQ Char(1) Inz(*Blanks);                                                          
       Dcl-s SpgcamQ Char(4) Inz(*Blanks);                                                          
       Dcl-s SpgvpsQ Packed(11:2) Inz(*Zeros);                                                      
       Dcl-s P_Acc Char(1) Inz(*Blanks);                                                            
       Dcl-s P_Cia Char(3) Inz(*Blanks);                                                            
       Dcl-s P_Zon Char(3) Inz(*Blanks);                                                            
       Dcl-s P_Ced Packed(15:0) Inz(*Zeros);                                                        
       Dcl-s P_Sec Char(3) Inz(*Blanks);                                                            
       Dcl-s P_Obs Char(250) Inz(*Blanks);                                                          
       Dcl-s P_User Char(10) Inz(*Blanks);                                                          
       Dcl-s Hrol Char(2) Inz(*Blanks);                                                             
       Dcl-s Hfec Char(10) Inz(*Blanks);                                                            
       Dcl-s Htexto Char(200) Inz(*Blanks);                                                         
       Dcl-s HtextoC Char(200) Inz(*Blanks);                                                        
       Dcl-s Wtexto Char(200) Inz(*Blanks);                                                         
                                                                                                    
      ** Definición de Procedimientos Parametros de Entrada                                         
       Dcl-Pr Main Extpgm('VTPSGB1R');                                                              
        Compa Char(3);                                                                              
        Ezona Char(3);                                                                              
        Ecampa Char(4);                                                                             
       End-Pr;                                                                                      
       Dcl-PI Main;                                                                                 
        Compa Char(3);                                                                              
        Ezona Char(3);                                                                              
        Ecampa Char(4);                                                                             
       End-PI;                                                                                      
                                                                                                    
       Dcl-Pr PrCompaÑias End-Pr;                                                                   
       Dcl-Pr PrGuiasActivas End-Pr;                                                                
       Dcl-Pr PrLider Char(10) End-Pr;                                                              
       Dcl-Pr PrEnviarEmail End-Pr;                                                                 
       Dcl-Pr PrInformacionCp End-Pr;                                                               
       Dcl-Pr PrLiderDet Char(1000) End-Pr;                                                         
       Dcl-Pr PrRegional Char(1000) End-Pr;                                                         
       Dcl-Pr PrComercial Char(1000) End-Pr;                                                        
       Dcl-Pr PrPagosPendientesGuias End-Pr;                                                        
       Dcl-Pr PrPagosPendientesGuiasRut End-Pr;                                                     
       Dcl-Pr PgmNewPlanG Extpgm('VTMPGB0R');                                                       
        Icia Char(3);                                                                               
        Izona Char(3);                                                                              
        Icaso Char(7);                                                                              
        Icedula Char(30);                                                                           
        Ifecha Char(10);                                                                            
        Orespuesta Char(1);                                                                         
       End-Pr;                                                                                      
       Dcl-Pr PrPoliticaRUT End-Pr;                                                                 
       Dcl-Pr PgmCampa Extpgm('VTMCSBKR');                                                          
        Pcia Char(3);                                                                               
        PCamE Char(4);                                                                              
        PCamR Char(4);                                                                              
        POper Char(1);                                                                              
        Pnume Char(3);                                                                              
       End-Pr;                                                                                      
       Dcl-Pr PgmCampaAct Extpgm('VTMCSC0R');                                                       
        Pcia Char(3);                                                                               
        PCamAct Char(4);                                                                            
       End-Pr;                                                                                      
       Dcl-Pr PgmConversion Extpgm('VOTREP00/VTMCSBCR');                                            
        Icia Char(3);                                                                               
        IcedLar Char(30);                                                                           
        IcedCor Char(9);                                                                            
       End-Pr;                                                                                      
       Dcl-Pr PgmInactivarGuia Extpgm('VOTREP00/VTSPGS4R');                                         
        SpCiaInte Char(10);                                                                         
        SpIdl Char(30);                                                                             
        SpCamp Char(4);                                                                             
        SpZong Char(3);                                                                             
        SpIdg Packed(15:0);                                                                         
        SpSect Char(1);                                                                             
        SpTerr Char(1);                                                                             
        SpObser Char(250);                                                                          
        SpAcc Char(1);                                                                              
        Spsts Char(1);                                                                              
        SpDes Char(120);                                                                            
       End-Pr;                                                                                      
       Dcl-Pr PgmCompaÑia Extpgm('VOTREP00/VTMCSBFR');                                              
        Kcia Char(10);                                                                              
        Kres Char(2);                                                                               
        KciaR Char(10);                                                                             
       End-Pr;                                                                                      
       Dcl-Pr ValidarEmail Extpgm('VOTREP00/VTMGOBTR');                                             
        PrtStatus Char(8);                                                                          
        Pemail Char(200);                                                                           
       End-Pr;                                                                                      
       Dcl-Pr EnviaCorreo Extpgm('VOTREP00/VTEPWB3R');                                              
        Pcia Char(3);                                                                               
        Pres Char(2);                                                                               
        Premitente Char(200);                                                                       
        Pdestinatario Char(200);                                                                    
        PRemMostrar Char(1000);                                                                     
        PAsunto Char(1000);                                                                         
        Pmensaje Char(32767);                                                                       
        PReplyto Char(10000);                                                                       
        Pcc Char(10000);                                                                            
        Pcco Char(10000);                                                                           
        Soapr Char(32767);                                                                          
       End-Pr;                                                                                      
       Dcl-Pr PgmHistoriaGs Extpgm('VOTREP00/VTPSGB3R');                                            
        P_Acc Char(1);                                                                              
        P_Cia Char(3);                                                                              
        P_Zon Char(3);                                                                              
        P_Ced Packed(15:0);                                                                         
        P_Sec Char(3);                                                                              
        P_Obs Char(250);                                                                            
        P_User Char(10);                                                                            
       End-Pr;                                                                                      
       Dcl-Pr PgmNombreComerciales Extpgm('VOTREP00/VTPLDITR');                                     
        Pcia Char(3);                                                                               
        Hrol Char(2);                                                                               
        Hfec Char(10);                                                                              
        Htexto Char(200);                                                                           
       End-Pr;                                                                                      
       Dcl-Pr PgmPoliticasRUT Extpgm('VOTREP00/VTMPGB3R');                                          
        Pcia Char(3);                                                                               
        Izona Char(3);                                                                              
        Icedula Char(30);                                                                           
        OrespuestaPgoRUT Char(1);                                                                   
        OrespuestaIngCnt Char(1);                                                                   
       End-Pr;                                                                                      
                                                                                                    
       //------------------------------------------------------------------/                        
       // Inicio Programa Principal                                        /                        
       //------------------------------------------------------------------/                        
       PrGuiasActivas();                                                                            
       *Inlr = *On;                                                                                 
       Return;                                                                                      
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Lee Archivo de Guías por País                  //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrGuiasActivas;                                                                     
                                                                                                    
       Exec Sql                                                                                     
        Declare Cur1 Cursor For                                                                     
        Select Gldcte, Gldcin, Gldzon, Gldsct, Gldtrr, Gldusr                                       
        From Votrea00/Vtmgldf                                                                       
        Where Gldest = ' ' And Gldcia = :Compa And                                                  
              Gldzon = :Ezona                                                                       
        Order By Gldcin, Gldcte;                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Open Cur1;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur1 Into :Cedula, :CmpIng, :Wzona, :Wsector, :Wterr, :Wuser;                         
                                                                                                    
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
        Icedula = %Trim(%Editc(Cedula:'Z'));                                                        
        Icaso = *Blanks;                                                                            
        Ifecha = *Blanks;                                                                           
        Orespuesta = *Blanks;                                                                       
        PgmNewPlanG(Compa:Wzona:Icaso:Icedula:Ifecha:Orespuesta);                                   
        If Orespuesta  = 'S';                                                                       
         PrPoliticaRUT();                                                                           
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch Cur1 Into :Cedula, :CmpIng, :Wzona, :Wsector, :Wterr, :Wuser;                        
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cur1;                                                                                 
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Lee Archivo de Politicas del RUT               //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrPoliticaRUT;                                                                      
        Dcl-s Nro Packed(3:0) Inz(*Zeros);                                                          
        Dcl-s NroCp Packed(3:0) Inz(*Zeros);                                                        
        Dcl-s Idx Packed(3:0) Inz(*Zeros);                                                          
        Dcl-s Ycampa Char(4) Inz(*Blanks);                                                          
        Dcl-s Yesta Char(1) Inz(*Blanks);                                                           
        Dcl-s PCamAct Char(4) Inz(*Blanks);                                                         
        Dcl-s IndAlf Char(1) Inz('N');                                                              
        Dcl-s SpCiaInte Char(10) Inz(*Blanks);                                                      
        Dcl-s SpIdl Char(30) Inz(*Blanks);                                                          
        Dcl-s SpCamp Char(4) Inz(*Blanks);                                                          
        Dcl-s SpZong Char(3) Inz(*Blanks);                                                          
        Dcl-s SpIdg Packed(15:0) Inz(*Zeros);                                                       
        Dcl-s SpSect Char(1) Inz(*Blanks);                                                          
        Dcl-s SpTerr Char(1) Inz(*Blanks);                                                          
        Dcl-s SpObser Char(250) Inz(*Blanks);                                                       
        Dcl-s SpAcc Char(1) Inz(*Blanks);                                                           
        Dcl-s Spsts Char(1) Inz(*Blanks);                                                           
        Dcl-s SpDes Char(120) Inz(*Blanks);                                                         
                                                                                                    
       CamRutV = *Zeros;                                                                            
       Exec Sql                                                                                     
        Select Pgrnro Into :Nro                                                                     
        From Votrea00/Vtmpgrf                                                                       
        Where Pgrest = ' ' And Pgrcia = :Compa;                                                     
                                                                                                    
       If Nro > *Zeros;                                                                             
        Nro += 1;                                                                                   
        PgmCampaAct(Compa:PCamAct);                                                                 
        PCamE = CmpIng;                                                                             
        PCamR = *Blanks;                                                                            
        Pnume = '000';                                                                              
                                                                                                    
        IcedLar = *Blanks;                                                                          
        IcedCor = %Trim(%Editc(Cedula:'Z'));                                                        
        PgmConversion(Compa:IcedLar:IcedCor);                                                       
        NroCp = *Zeros;                                                                             
        Exec Sql                                                                                    
         Select Count(0) Into :NroCp                                                                
         From Votrea00/Vtmrtgf                                                                      
         Where Rtgcia = :Compa And (Rtgidt = :Cedula Or                                             
               Trim(Char(Rtgidt)) = Trim(:IcedLar))                                                 
               And Rtgest = ' ';                                                                    
                                                                                                    
        If NroCp <= *Zeros;                                                                         
         Exec Sql                                                                                   
          Select Digits(:Nro) Into :Pnume                                                           
          From Sysibm/Sysdummy1;                                                                    
                                                                                                    
         POper = '+';                                                                               
         PgmCampa(Compa:PCamE:PCamR:POper:Pnume);                                                   
         CamRutV = PCamR;                                                                           
                                                                                                    
         IndAlf = 'S';                                                                              
         If Ecampa >= CamRutV;                                                                      
          IndAlf = 'N';                                                                             
         Endif;                                                                                     
                                                                                                    
         If IndAlf = 'N';                                                                           
          Ycampa = *Blanks;                                                                         
          Yesta = *Blanks;                                                                          
          Exec Sql                                                                                  
           Select Estcam, Estest Into :Ycampa, :Yesta                                               
           From Orderp00/Opiestf1                                                                   
           Where Estcia = :Compa And                                                                
                 Estcam = :Ecampa And                                                               
                 Estzon = :Ezona                                                                    
           Order By Rrn(Opiestf1) Desc                                                              
           Fetch First 1 Row Only;                                                                  
                                                                                                    
          If Yesta = 'S';                                                                           
           If Ycampa = Ecampa;                                                                      
                                                                                                    
            Exec Sql                                                                                
             Declare CurT Cursor For                                                                
             Select Spgcia, Spgzon, Spgced, Spgsec, Spgcam, Spgvps                                  
             From Votrea00/Vtmspgf                                                                  
             Where Spgcia = :Compa And                                                              
                   Spgzon = :Ezona And                                                              
                   Spgced = :Cedula And                                                             
                   (Spginp = ' ' Or Spginp = 'E');                                                  
                                                                                                    
            Exec Sql                                                                                
             Open CurT;                                                                             
                                                                                                    
            Exec Sql                                                                                
             Fetch CurT Into :SpgciaQ, :SpgzonQ, :SpgcedQ, :SpgsecQ,                                
                             :SpgcamQ, :SpgvpsQ;                                                    
                                                                                                    
            Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                 
             Exec Sql                                                                               
              Update Votrea00/Vtmspgf Set Spginp = 'N'                                              
              Where Spgcia = :SpgciaQ And                                                           
                    Spgzon = :SpgzonQ And                                                           
                    Spgced = :SpgcedQ And                                                           
                    (Spginp = ' ' Or Spginp = 'E');                                                 
                                                                                                    
             If SqlCode = *Zeros;                                                                   
               PrPagosPendientesGuias();                                                            
             EndIf;                                                                                 
             Exec Sql                                                                               
              Fetch CurT Into :SpgciaQ, :SpgzonQ, :SpgcedQ, :SpgsecQ,                               
                              :SpgcamQ, :SpgvpsQ;                                                   
            Enddo;                                                                                  
                                                                                                    
            Exec Sql                                                                                
             Close CurT;                                                                            
                                                                                                    
            Kcia = Compa;                                                                           
            Kres = ' ';                                                                             
            KCiaR = *Blanks;                                                                        
            PgmCompaÑia(Kcia:Kres:KCiaR);                                                           
                                                                                                    
            Hrol = 'GS';                                                                            
            Htexto = *Blanks;                                                                       
            PgmNombreComerciales(SpgciaQ:Hrol:Hfec:Htexto);                                         
            Hrol = 'CC';                                                                            
            HtextoC = *Blanks;                                                                      
            PgmNombreComerciales(SpgciaQ:Hrol:Hfec:HtextoC);                                        
            Wtexto = *Blanks;                                                                       
            If %Len(%Trim(HtextoC)) <= *Zeros;                                                      
             Wtexto = 'Compradora';                                                                 
            Else;                                                                                   
             Wtexto = %Trim(HtextoC);                                                               
            Endif;                                                                                  
            SPciaInte = KCiaR;                                                                      
            SpIdl = PrLider();                                                                      
            SpCamp = PCamAct;                                                                       
            SpZong = Wzona;                                                                         
            SpIdg = Cedula;                                                                         
            SpSect = Wsector;                                                                       
            SpTerr = Wterr;                                                                         
            SpObser = %Trim(Wtexto) +                                                               
                      ' activa como ' + %Trim(Htexto) + ' en C'                                     
                      + %Trim(CmpIng) +                                                             
                      ', en ' + %Trim(%Editc((Nro-1):'Z')) + ' Campañas' +                          
                      ' se espero el envió del RUT pero este no fue envi' +                         
                      'ado de acuerdo al plan, se inactivo en C' +                                  
                      %Trim(CamRutV) + '. Por ser documento legal el des' +                         
                      'cuento legal tributario el descuento no fue posib' +                         
                      'le realizarlo.';                                                             
            SpAcc = 'D';                                                                            
            Spsts = *Blanks;                                                                        
            SpDes = *Blanks;                                                                        
            PrEnviarEmail();                                                                        
            // Se Invoca Programa que Inactiva Guía en el Sistema                                   
            PgmInactivarGuia(SPciaInte                                                              
                             :SpIdl                                                                 
                             :SpCamp                                                                
                             :SpZong                                                                
                             :SpIdg                                                                 
                             :SpSect                                                                
                             :SpTerr                                                                
                             :SpObser                                                               
                             :SpAcc                                                                 
                             :Spsts                                                                 
                             :SpDes);                                                               
                                                                                                    
            // Se Invoca Programa que Graba Historia del Envio del Email                            
            P_Acc = 'N';                                                                            
            P_Cia = SpgciaQ;                                                                        
            P_Zon = Wzona;                                                                          
            P_Ced = Cedula;                                                                         
            P_Sec = Wsector;                                                                        
            P_Obs = %Trim(SpObser);                                                                 
            P_User = Wuser;                                                                         
            PgmHistoriaGs(P_Acc:P_Cia:P_Zon:P_Ced:P_Sec:P_Obs:P_User);                              
           Endif;                                                                                   
          Endif;                                                                                    
         Endif;                                                                                     
                                                                                                    
        Endif;                                                                                      
       Else;                                                                                        
        PgmCampaAct(Compa:PCamAct);                                                                 
        PCamE = CmpIng;                                                                             
        PCamR = *Blanks;                                                                            
        Pnume = '000';                                                                              
                                                                                                    
        IcedLar = *Blanks;                                                                          
        IcedCor = %Trim(%Editc(Cedula:'Z'));                                                        
        PgmConversion(Compa:IcedLar:IcedCor);                                                       
        NroCp = *Zeros;                                                                             
        Exec Sql                                                                                    
         Select Count(0) Into :NroCp                                                                
         From Votrea00/Vtmrtgf                                                                      
         Where Rtgcia = :Compa And (Rtgidt = :Cedula Or                                             
               Trim(Char(Rtgidt)) = Trim(:IcedLar))                                                 
               And Rtgest = ' ';                                                                    
                                                                                                    
        If NroCp <= *Zeros;                                                                         
         Exec Sql                                                                                   
          Update Votrea00/Vtmspgf Set Spginp = 'N'                                                  
          Where Spgcia = :SpgciaQ And                                                               
                Spgzon = :SpgzonQ And                                                               
                Spgced = :SpgcedQ And                                                               
                (Spginp = ' ' Or Spginp = 'E');                                                     
                                                                                                    
         If SqlCode = *Zeros;                                                                       
           PrPagosPendientesGuiasRut();                                                             
         EndIf;                                                                                     
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Lee Archivo de Líderes                         //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrLider;                                                                            
        Dcl-Pi *N Char(10) End-Pi;                                                                  
        Dcl-s SqlStr Char(1000) Inz(*Blanks);                                                       
        Dcl-s Lider Char(10) Inz(*Blanks);                                                          
                                                                                                    
        SqlStr = 'Select Trim(Sminf1) ' +                                                           
                 'From Opf' + Compa + '/Lslman01 ' +                                                
                 'Where Trim(Smslsp) = ' + '''' + Wzona + '''';                                     
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur2 From :SqlStr;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor2 Cursor For Cur2;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor2;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor2 Into :Lider;                                                                  
                                                                                                    
       If Sqlcod = 100 Or Sqlcod < *Zeros;                                                          
        Lider = *Blanks;                                                                            
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor2;                                                                              
                                                                                                    
       Return Lider;                                                                                
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Envia Correo a Personas del Area Comercial     //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrEnviarEmail;                                                                      
        Dcl-s Pcia Char(3) Inz(*Blanks);                                                            
        Dcl-s Pres Char(2) Inz(*Blanks);                                                            
        Dcl-s Premitente Char(200) Inz(*Blanks);                                                    
        Dcl-s Pdestinatario Char(200) Inz(*Blanks);                                                 
        Dcl-s PRemMostrar Char(1000) Inz(*Blanks);                                                  
        Dcl-s PAsunto Char(1000) Inz(*Blanks);                                                      
        Dcl-s Pmensaje Char(32767) Inz(*Blanks);                                                    
        Dcl-s PReplyto Char(10000);                                                                 
        Dcl-s Pcc Char(10000);                                                                      
        Dcl-s Pcco Char(10000);                                                                     
        Dcl-s Soapr Char(32767);                                                                    
        Dcl-s w_FechaC Char(10) Inz(*Blanks);                                                       
        Dcl-s W_FechaN Char(10) Inz(*Blanks);                                                       
                                                                                                    
       // Asigna compañia                                                                           
       Pcia= Compa;                                                                                 
       // Asigna Respuesta                                                                          
       Pres= *Blanks;                                                                               
       // Asigna Remitente                                                                          
       Premitente = 'servicioalcliente@leonisa.com';                                                
       Wnombre = *Blanks;                                                                           
       If %Trim(IcedLar) <> '0';                                                                    
        PrInformacionCp();                                                                          
       Endif;                                                                                       
       // Destinatarios a Enviar                                                                    
       Pdestinatario = *Blanks;                                                                     
       Wlider = PrLiderDet();                                                                       
       Wregional = PrRegional();                                                                    
       Wcomercial = PrComercial();                                                                  
       // Email Lider                                                                               
       If %Len(%Trim(Wlider)) > *Zeros;                                                             
        Pdestinatario = %Trim(Wlider);                                                              
       Endif;                                                                                       
        // Email Regional                                                                           
        If %Len(%Trim(Wregional)) > *Zeros;                                                         
         If %Len(%Trim(Pdestinatario)) <= *Zeros;                                                   
          Pdestinatario = %Trim(Wregional);                                                         
         Else;                                                                                      
          Pdestinatario = %Trim(Pdestinatario) + '|' + %Trim(Wregional);                            
         Endif;                                                                                     
        Endif;                                                                                      
       // Email Area Comercial                                                                      
       If %Len(%Trim(Wcomercial)) > *Zeros;                                                         
        If %Len(%Trim(Pdestinatario)) <= *Zeros;                                                    
         Pdestinatario = %Trim(Wcomercial);                                                         
        Else;                                                                                       
         Pdestinatario = %Trim(Pdestinatario) + '|' + %Trim(Wcomercial);                            
        Endif;                                                                                      
       Endif;                                                                                       
       // Se Activo la Guía Correctamente                                                           
        // Asigna Descripcion mensaje                                                               
        PRemMostrar= %Trim(Htexto) + ' inactiva por no enviar el RUT';                              
        // Asigna asunto                                                                            
        Pasunto    = %Trim(Htexto) + ' inactiva por no enviar el RUT';                              
        // Asigna asunto                                                                            
        Pmensaje   = 'Cordial saludo, <br><br>' +                                                   
        'La ' + %Trim(Wtexto) + ' ' + %Trim(Wnombre) + ' con CC ' +                                 
         %Trim(IcedLar) + ' fue inscrita en el plan de ' +                                          
         %Trim(Htexto) + ' en C' +                                                                  
         %Trim(CmpIng) + ' al cierre de la C' + %Trim(CamRutV) +                                    
         ' no fue recibido el RUT, por tal motivo ' +                                               
         'fue desactivada del plan. <br>' +                                                         
         'Los descuentos acumulados en el plan no fueron realizados ' +                             
         'ya que tributariamente era necesario el RUT para aplicar ' +                              
         'el ajuste.';                                                                              
       PReplyto   = *Blanks;                                                                        
       Pcc        = *Blanks;                                                                        
       Pcco       = *Blanks;                                                                        
       Soapr      = *Blanks;                                                                        
                                                                                                    
       EnviaCorreo(Pcia           :                                                                 
                   Pres            :                                                                
                   Premitente      :                                                                
                   Pdestinatario   :                                                                
                   PRemMostrar     :                                                                
                   PAsunto         :                                                                
                   Pmensaje        :                                                                
                   PReplyto        :                                                                
                   Pcc             :                                                                
                   Pcco            :                                                                
                   Soapr           );                                                               
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta la Información de la Compradora       //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrInformacionCp;                                                                    
        Dcl-s SqlStr Char(1000) Inz(*Blanks);                                                       
                                                                                                    
        Wnombre = *Blanks;                                                                          
        SqlStr = *Blanks;                                                                           
        SqlStr = 'Select Trim(Mlname) ' +                                                           
                 'From Opcapf/Pmllst' + Compa + ' ' +                                               
                 'Where MlcstÑ = ' + %Trim(%Editc(Cedula:'Z')) + ' ' +                              
                 'Fetch First 1 Row Only';                                                          
                                                                                                    
        Exec Sql                                                                                    
         Prepare CurA From :SqlStr;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Declare CursorA Cursor For CurA;                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open CursorA;                                                                              
                                                                                                    
        Exec Sql                                                                                    
         Fetch CursorA Into :Wnombre;                                                               
                                                                                                    
        If Sqlcod = 100 Or Sqlcod < *Zeros;                                                         
         Wnombre = *Blanks;                                                                         
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close CursorA;                                                                             
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Email de la Líder                  //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrLiderDet;                                                                         
        Dcl-Pi *N Char(1000) End-Pi;                                                                
        Dcl-s Email Char(200) Inz(*Blanks);                                                         
        Dcl-s Wlider Char(1000) Inz(*Blanks);                                                       
        Dcl-s SqlStr Char(1000) Inz(*Blanks);                                                       
                                                                                                    
        Region = *Blanks;                                                                           
        SqlStr = *Blanks;                                                                           
        SqlStr = 'Select Daleml, Sminf2 ' +                                                         
                 'From Vpcopa00/Vtmdalf Inner Join Opf' + Compa +                                   
                 '/Pslman On Trim(Sminf1) = Trim(Dalced) ' +                                        
                'Where Trim(Smslsp) = ' + '''' + Wzona + '''' + ' And ' +                           
                'Smdele = '' ''' + ' ' +                                                            
                'Fetch First 1 Row Only';                                                           
                                                                                                    
        Exec Sql                                                                                    
         Prepare CurB From :SqlStr;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Declare CursorB Cursor For CurB;                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open CursorB;                                                                              
                                                                                                    
        Exec Sql                                                                                    
         Fetch CursorB Into :Email, :Region;                                                        
                                                                                                    
        If Sqlcod = 100 Or Sqlcod < *Zeros;                                                         
         Email = *Blanks;                                                                           
         Region = *Blanks;                                                                          
        Endif;                                                                                      
                                                                                                    
       Respuesta = *Blanks;                                                                         
       w_Email = *Blanks;                                                                           
       w_Email = Email;                                                                             
       ValidarEmail(Respuesta:w_Email);                                                             
       If %Trim(Respuesta) = 'Invalido';                                                            
        Wlider = *Blanks;                                                                           
       Else;                                                                                        
        Wlider = Email;                                                                             
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CursorB;                                                                              
                                                                                                    
       Return Wlider;                                                                               
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Email del Regional                 //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrRegional;                                                                         
        Dcl-Pi *N Char(1000) End-Pi;                                                                
        Dcl-s Email Char(200) Inz(*Blanks);                                                         
        Dcl-s Wregional Char(1000) Inz(*Blanks);                                                    
        Dcl-s SqlStr Char(1000) Inz(*Blanks);                                                       
                                                                                                    
        SqlStr = *Blanks;                                                                           
        SqlStr = 'Select Divemd ' +                                                                 
                 'From Votrea00/Vtmdivf ' +                                                         
                 'Where Trim(Divpai) = ' + '''' + Compa + '''' + ' And '+                           
                'Divest = '' ''' + ' And Divdiv =' + '''' + %Trim(Region)                           
                 + ''' ' + ' ' +                                                                    
                 'Fetch First 1 Row Only';                                                          
                                                                                                    
        Exec Sql                                                                                    
         Prepare CurC From :SqlStr;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Declare CursorC Cursor For CurC;                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open CursorC;                                                                              
                                                                                                    
        Exec Sql                                                                                    
         Fetch CursorC Into :Email;                                                                 
                                                                                                    
        If Sqlcod = 100 Or Sqlcod < *Zeros;                                                         
         Email = *Blanks;                                                                           
        Endif;                                                                                      
                                                                                                    
        Respuesta = *Blanks;                                                                        
        w_Email = Email;                                                                            
        ValidarEmail(Respuesta:w_Email);                                                            
        If %Trim(Respuesta) = 'Invalido';                                                           
         Wregional = *Blanks;                                                                       
        Else;                                                                                       
         Wregional = Email;                                                                         
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close CursorC;                                                                             
                                                                                                    
        Return Wregional;                                                                           
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Email del Area Comercial           //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrComercial;                                                                        
        Dcl-Pi *N Char(1000) End-Pi;                                                                
        Dcl-s Email Char(200) Inz(*Blanks);                                                         
        Dcl-s Wcomercial Char(1000) Inz(*Blanks);                                                   
        Dcl-s SqlStr Char(1000) Inz(*Blanks);                                                       
        Dcl-s Nro Packed(3:0) Inz(*Zeros);                                                          
        Dcl-s Coma Char(1) Inz(',');                                                                
                                                                                                    
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Seceml ' +                                                                  
                'From Votrea00/Vtmsecf ' +                                                          
                'Where Trim(Seccia) = ' + '''' + Compa + '''' + ' And ' +                           
                'Trim(Secdiv) = ' + '''' + %Trim(Region) + '''' + ' And ' +                         
                'Secest = '' ''';                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Prepare CurD From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare CursorD Cursor For CurD;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open CursorD;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch CursorD Into :Email;                                                                  
                                                                                                    
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
        Respuesta = *Blanks;                                                                        
        w_Email = Email;                                                                            
        ValidarEmail(Respuesta:w_Email);                                                            
        If %Trim(Respuesta) <> 'Invalido';                                                          
         Nro += 1;                                                                                  
         If Nro > 1;                                                                                
          Coma = ',';                                                                               
         Else;                                                                                      
          Coma = *Blanks;                                                                           
         Endif;                                                                                     
         WComercial = %Trim(WComercial) + %Trim(Email) + %Trim(Coma);                               
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch CursorD Into :Email;                                                                 
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CursorD;                                                                              
                                                                                                    
       Return WComercial;                                                                           
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba los Casos Pendientes Pago Guía por Falta //                       
       // del RUT                                                          //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrPagosPendientesGuias;                                                             
        Dcl-s User Char(10) Inz(*User);                                                             
        Dcl-s Nro Packed(3:0) Inz(*Zeros);                                                          
        Dcl-s Tvalor Packed(11:2) Inz(*Zeros);                                                      
        Dcl-s OrespuestaPgoRUT Char(1) Inz('N');                                                    
        Dcl-s OrespuestaIngCnt Char(1) Inz('N');                                                    
                                                                                                    
        IcedLar = *Blanks;                                                                          
        IcedCor = %Trim(%Editc(SpgcedQ:'Z'));                                                       
        PgmConversion(SpgciaQ:IcedLar:IcedCor);                                                     
        PgmPoliticasRUT(SpgciaQ:SpgzonQ:IcedLar:OrespuestaPgoRUT:                                   
                        OrespuestaIngCnt);                                                          
        // Si Aplica Política Nuevo Plan de Guías                                                   
        If OrespuestaPgoRUT = 'S';                                                                  
        Clear *Nokey Regppr;                                                                        
        Chain (SpgciaQ:SpgcamQ:SpgzonQ:SpgsecQ:SpgcedQ) Vttpprf;                                    
        If Not %Found(Vttpprf);                                                                     
         Pprcia = SpgciaQ;                                                                          
         Pprcmp = SpgcamQ;                                                                          
         Pprzna = SpgzonQ;                                                                          
         Pprsec = SpgsecQ;                                                                          
         Pprcdg = SpgcedQ;                                                                          
                                                                                                    
         Tvalor = *Zeros;                                                                           
         Exec Sql                                                                                   
          Select Sum(Spgvps) Into :Tvalor                                                           
          From Votrea00/Vtmspgf                                                                     
          Where Spgcia = :SpgciaQ And                                                               
                Spgzon = :SpgzonQ And                                                               
                Spgcam = :SpgcamQ And                                                               
                Spgced = :SpgcedQ And                                                               
                Spgsec = :SpgsecQ;                                                                  
                                                                                                    
         Pprvlr = Tvalor;                                                                           
         Pprest = 'N';                                                                              
                                                                                                    
         Exec Sql                                                                                   
          Select Pgrnro Into :Nro                                                                   
          From Votrea00/Vtmpgrf                                                                     
          Where Pgrest = ' ' And Pgrcia = :SpgciaQ;                                                 
                                                                                                    
          Pprmot = 'Descuento NO Realizado ' + %Trim(Htexto) +                                      
                   ' Inactiva RUT no Enviado';                                                      
          Pprdes = %Trim(Wtexto)  + ' activa en ' + %Trim(Htexto) +                                 
                   ' en C' + %Trim(CmpIng) +                                                        
                   ', en ' + %Trim(%Editc((Nro-1):'Z')) + ' Campañas' +                             
                   ' se espero el envió del RUT pero este no fue envi' +                            
                   'ado de acuerdo al plan, se inactivo en C' +                                     
                   %Trim(SpgcamQ) + '. Por ser documento legal el des' +                            
                   'cuento legal tributario el descuento no fue posib' +                            
                   'le realizarlo.';                                                                
          Pprfec = %Char(%Date());                                                                  
          Pprhor = %Char(%Time():*Hms);                                                             
          Pprusr = User;                                                                            
          Write Regppr;                                                                             
          //Se agrega la actualizacion al archivo Vtmspgf en el indicador de Posteo En 'E'          
          Exec Sql                                                                                  
            Update Votrea00/Vtmspgf Set SpgInp = 'E'                                                
            Where SpgCia = :SpgCiaQ And                                                             
                  SpgZon = :SpgzonQ And                                                             
                  SpgCam = :SpgcamQ And                                                             
                  SpgSec = :SpgsecQ And                                                             
                  SpgTrr = :Wterr And                                                               
                  SpgCed = :SpgcedQ;                                                                
        Endif;                                                                                      
        Endif;                                                                                      
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba los Casos Pendientes Pago Guía por Falta //                       
       // del RUT                                                          //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrPagosPendientesGuiasRut;                                                          
        Dcl-s User Char(10) Inz(*User);                                                             
        Dcl-s Nro Packed(3:0) Inz(*Zeros);                                                          
        Dcl-s Tvalor Packed(11:2) Inz(*Zeros);                                                      
        Dcl-s OrespuestaPgoRUT Char(1) Inz('N');                                                    
        Dcl-s OrespuestaIngCnt Char(1) Inz('N');                                                    
                                                                                                    
        IcedLar = *Blanks;                                                                          
        IcedCor = %Trim(%Editc(Cedula:'Z'));                                                        
        PgmConversion(Compa:IcedLar:IcedCor);                                                       
        PgmPoliticasRUT(Compa:Wzona:IcedLar:OrespuestaPgoRUT:                                       
                        OrespuestaIngCnt);                                                          
        // Si Aplica Política Nuevo Plan de Guías                                                   
        If OrespuestaPgoRUT = 'S';                                                                  
        Clear *Nokey Regppr;                                                                        
        Chain (Compa:Ecampa:Ezona:Wsector:Cedula) Vttpprf;                                          
        If Not %Found(Vttpprf);                                                                     
         Pprcia = Compa;                                                                            
         Pprcmp = Ecampa;                                                                           
         Pprzna = Wzona;                                                                            
         Pprsec = Wsector;                                                                          
         Pprcdg = Cedula;                                                                           
         SpgvpsQ = *Zeros;                                                                          
                                                                                                    
         Tvalor = *Zeros;                                                                           
         Exec Sql                                                                                   
          Select Sum(Spgvps) Into :Tvalor                                                           
          From Votrea00/Vtmspgf                                                                     
          Where Spgcia = :Compa And                                                                 
                Spgzon = :Ezona And                                                                 
                Spgcam = :Ecampa And                                                                
                Spgced = :Cedula And                                                                
                Spgsec = :Wsector;                                                                  
                                                                                                    
         Pprvlr = Tvalor;                                                                           
         Pprest = 'E';                                                                              
         Pprmot = 'Descuento NO Realizado RUT no Enviado';                                          
         Pprdes = 'Pago NO realizado a la ' + %Trim(Htexto) +                                       
                  ' en C' + %Trim(Ecampa) +                                                         
                  ' porque no se tiene la información del RUT.';                                    
         Pprfec = %Char(%Date());                                                                   
         Pprhor = %Char(%Time():*Hms);                                                              
         Pprusr = User;                                                                             
         Write Regppr;                                                                              
                                                                                                    
          //Se agrega la actualizacion al archivo Vtmspgf en el indicador de Posteo En 'E'          
          Exec Sql                                                                                  
            Update Votrea00/Vtmspgf Set SpgInp = 'E'                                                
            Where SpgCia = :Compa And                                                               
                  SpgZon = :EZona And                                                               
                  SpgCam = :ECampa And                                                              
                  SpgSec = :WSector And                                                             
                  SpgTrr = :Wterr And                                                               
                  SpgCed = :Cedula;                                                                 
                                                                                                    
        Endif;                                                                                      
        Endif;                                                                                      
       End-Proc;                                                                                    
