      ***********************************************************************                       
      ** Programa      : VTPSGC7R                                          **                       
      ** Descripciòn   : PGM que Verifica si se debe Recalcular el Cupo    **                       
      **                 Base de una Líder por Traslados de Compradoras    **                       
      ** Autor         : (JOrtega)                                         **                       
      ** Fecha Creación: 30 de Abril de 2017                               **                       
      ***********************************************************************                       
      ** Definición de Directivas de Compilación                                                    
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     DXreg             s             20    Inz(*Blanks)                                             
     DWcedula          s              9  0 Inz(*Zeros)                                              
     DNro              s              5  0 Inz(*Zeros)                                              
     DTipo             s              1a   Inz(*Blanks)                                             
     DPnro             s              4  0 Inz(*Zeros)                                              
     DCampa1           s              4a   Inz(*Blanks)                                             
     DCampa2           s              4a   Inz(*Blanks)                                             
     DPCamE            s              4a   Inz(*Blanks)                                             
     DPCamR            s              4a   Inz(*Blanks)                                             
     DPOper            s              1a   Inz(*Blanks)                                             
     DPnume            s              3a   Inz(*Blanks)                                             
     DRcbcntW          s              4  0 Inz(*Zeros)                                              
     DRcboprW          s              2a   Inz(*Blanks)                                             
     DRcbactW          s              5  2 Inz(*Zeros)                                              
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DIcaso            s              7a   Inz(*Blanks)                                             
     DIfecha           s             10a   Inz(*Blanks)                                             
     DOrespuesta       s              1a   Inz(*Blanks)                                             
     DTced             s             30a   Inz(*Blanks)                                             
     DOperador         s              1a   Inz(*Blanks)                                             
     DPvar             s              1a   Inz(*Blanks)                                             
     DWzona            s              3a   Inz(*Blanks)                                             
     DWsect            s              1a   Inz(*Blanks)                                             
     DWdes             s           1000a   Inz(*Blanks)                                             
     DTsw              s              1a   Inz('N')                                                 
     DOperacion        s              5  0 Inz(*Zeros)                                              
     DDiferencia       s              5  0 Inz(*Zeros)                                              
     DPbase            s              4  0 Inz(*Zeros)                                              
     DPremitente       s            200a   Inz(*Blanks)                                             
     DPdestinatario    s            200a   Inz(*Blanks)                                             
     DPRemMostrar      s           1000a   Inz(*Blanks)                                             
     DPAsunto          s           1000a   Inz(*Blanks)                                             
     DPReplyto         s          10000a   Inz(*Blanks)                                             
     DPmensaje         s          32767a   Inz(*Blanks)                                             
     DPcc              s          10000a   Inz(*Blanks)                                             
     DPcco             s          10000a   Inz(*Blanks)                                             
     DPres             s              2a   Inz(*Blanks)                                             
     DRespuesta        s              8a   Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DEmail            s            200a   Inz(*Blanks)                                             
     Dw_EMAIL          s            200a   Inz(*Blanks)                                             
     DSoapr            s          32767a   Inz(*Blanks)                                             
                                                                                                    
     DMain             Pr                  Extpgm('VTPSGC7R')                                       
     DPcia                            3a                                                            
     DPzona                           3a                                                            
     DPcamp                           4a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DPzona                           3a                                                            
     DPcamp                           4a                                                            
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DPrRecalcularCupoBaseVtmnpif...                                                                
     D                 Pr                                                                           
     DPrRecalcularCupoBaseVttepmf...                                                                
     D                 Pr                                                                           
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
     DPrHistoria       Pr                                                                           
     DPrEnviaCorreo    Pr                                                                           
     DValidarEmail     Pr                  Extpgm('VOTREP00/VTMGOBTR')                              
     DPrtStatus                       8a                                                            
     DPemail                        200a                                                            
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DEnviaCorreo      Pr                  Extpgm('VOTREP00/VTEPWB3R')                              
     DPcia                            3a                                                            
     DPres                            2a                                                            
     DPremitente                    200a                                                            
     DPdestinatario                 200a                                                            
     DPRemMostrar                  1000a                                                            
     DPAsunto                      1000a                                                            
     DPmensaje                    32767a                                                            
     DPReplyto                    10000a                                                            
     DPcc                         10000a                                                            
     DPcco                        10000a                                                            
     DSoapr                       32767a                                                            
     DPrRegional       Pr          1000a                                                            
     DPrLider          Pr          1000a                                                            
     DPrNombre         Pr            60a                                                            
     DPcedula                         9  0                                                          
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       PgmNewPlanG(Pcia:Pzona:Icaso:Tced:Ifecha:Orespuesta);                                        
       If Orespuesta = 'S';                                                                         
       PCamE = Pcamp;                                                                               
       PCamR = *Blanks;                                                                             
       POper = '-';                                                                                 
       Pnume = '001';                                                                               
       If Pcamp = '2010';                                                                           
        Pnume = '002';                                                                              
       Endif;                                                                                       
       proCampa(Pcia:PCamE:PCamR:POper:Pnume);                                                      
       Campa1 = PcamR;                                                                              
       Campa2 = Pcamp;                                                                              
        // Consulta el Valor a Realizar el Recalculo del Cupo Base                                  
       Exec Sql                                                                                     
        Declare Cur Cursor For                                                                      
        Select Rcbcnt, Rcbopr, Rcbact                                                               
        From Votrea00/Vtmrcbf                                                                       
        Where Rcbcia = :Pcia And                                                                    
              Rcbest = ' '                                                                          
        Order By Rcbcnt Desc;                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Open Cur;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur Into :RcbcntW, :RcboprW, :RcbactW;                                                
                                                                                                    
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
        // Resta al Cupo Base de la Líder                                                           
        If %Trim(RcboprW) = '<' Or %Trim(RcboprW) = '<=';                                           
         Operador = '-';                                                                            
         Pvar = 'V';                                                                                
        Endif;                                                                                      
                                                                                                    
        // Suma al Cupo Base de la Líder                                                            
        If %Trim(RcboprW) = '>' Or %Trim(RcboprW) = '>=';                                           
         Operador = '+';                                                                            
         Pvar = 'N';                                                                                
        Endif;                                                                                      
                                                                                                    
        SqlStr = *Blanks;                                                                           
        SqlStr = 'Select Distinct(Count(Tzaced)), Max(Tzatpo),  ' +                                 
                 'Max(Tzazna), Max(Tzasct) ' +                                                      
                 'From Votrea00/Vtwtzaf ' +                                                         
                 'Where Tzacia = ' + '''' + Pcia + '''' + ' And ' +                                 
                 'Tzazna = ' + '''' + Pzona + '''' + ' And ' +                                      
                 'Tzatpo = ' + '''' + Pvar + '''' + ' And ' +                                       
                 'Tzacmp Between ' + '''' + Campa1 + '''' +                                         
                 ' And ' + '''' + Campa2 + '''' + ' And ' +                                         
                 'Tzapro = '' ''' + ' Group By Tzazna, Tzatpo, ' +                                  
                 'Tzazna, Tzasct ' +                                                                
                 'Having Count(Tzaced)' + %Trim(RcboprW) + ' ' +                                    
                  %Trim(%Editc(RcbcntW:'Z'));                                                       
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur0 From :SqlStr;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor0 Cursor For Cur0;                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor0;                                                                              
                                                                                                    
        Exec Sql                                                                                    
         Fetch Cursor0 Into :Nro, :Tipo, :Wzona, :Wsect;                                            
                                                                                                    
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         Operacion = *Zeros;                                                                        
         Diferencia = *Zeros;                                                                       
         Pbase = *Zeros;                                                                            
         Wcedula = *Zeros;                                                                          
         Exec Sql                                                                                   
          Select Gldcte Into :Wcedula                                                               
          From Votrea00/Vtmgldf                                                                     
          Where Gldcia = :Pcia And Gldzon = :Pzona And                                              
                Gldsct = :Wsect And Gldest = ' '                                                    
          Fetch First 1 Row Only;                                                                   
                                                                                                    
         If SqlCod <> 100 And SqlCod >= *Zeros;                                                     
          Tsw = 'N';                                                                                
          PrRecalcularCupoBaseVtmnpif();                                                            
          PrRecalcularCupoBaseVttepmf();                                                            
         Endif;                                                                                     
                                                                                                    
         If Tsw = 'S';                                                                              
          Tsw = 'N';                                                                                
          Exec Sql                                                                                  
           Update Votrea00/Vtwtzaf Set Tzapro = 'P'                                                 
           Where Tzacia = :Pcia And                                                                 
                 Tzazna = :Wzona And                                                                
                 Tzasct = :Wsect And                                                                
                 Tzatpo = :Pvar And                                                                 
                 Tzapro = ' ' And                                                                   
                 Tzacmp Between :Campa1 And :Campa2;                                                
         Endif;                                                                                     
                                                                                                    
         Exec Sql                                                                                   
          Fetch Cursor0 Into :Nro, :Tipo, :Wzona, :Wsect;                                           
        Enddo;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor0;                                                                             
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur Into :RcbcntW, :RcboprW, :RcbactW;                                                
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cur;                                                                                  
                                                                                                    
       Endif;                                                                                       
       Return;                                                                                      
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Recalcula el Cupo Base de la Líder Vtmnpif     //                       
       //------------------------------------------------------------------//                       
     PPrRecalcularCupoBaseVtmnpif...                                                                
     P                 B                                                                            
     DPrRecalcularCupoBaseVtmnpif...                                                                
     D                 Pi                                                                           
      /Free                                                                                         
       Exec Sql                                                                                     
        Select Npinro Into :Pbase                                                                   
        From Votrea00/Vtmnpif                                                                       
        Where Npicia = :Pcia And                                                                    
              Npizna = :Wzona And                                                                   
              Npiest = ' ' And                                                                      
              Npicdg = :Wcedula And                                                                 
              Npisec = :Wsect And                                                                   
              Npitpo = 'A';                                                                         
                                                                                                    
       If SqlCod <> 100 And SqlCod >= *Zeros;                                                       
       SqlStr = *Blanks;                                                                            
       Eval(h) Operacion = (Nro * (RcbactW/100));                                                   
       Select;                                                                                      
        When Operador = '+';                                                                        
         Diferencia = *Zeros;                                                                       
         Diferencia = Pbase + Operacion;                                                            
         Wdes = 'Modificación cupo base por traslado de #' +                                        
                 %Trim(%Editc(Operacion:'J')) + ' ejecutivas a su ' +                               
                'sector, con actividad ' + %Trim(%Editc(RcbactW:'J')) +                             
                '%, aumento cupo base de ' + %Trim(%Editc(Pbase:'J')) +                             
                ' a ' + %Trim(%Editc(Diferencia:'J')) + ' pedidos.';                                
         SqlStr = 'Update Votrea00/Vtmnpif Set Npinro = Npinro + ' +                                
                  %Trim(%Editc(Operacion:'Z')) + ' Where ' +                                        
                  'Npicia = ' + '''' + Pcia + '''' + ' And ' +                                      
                  'Npizna = ' + '''' + Wzona + '''' + ' And ' +                                     
                  'Npicdg = ' + %Trim(%Editc(Wcedula:'Z')) + ' And ' +                              
                  'Npisec = ' + '''' + Wsect + '''' + ' And ' +                                     
                  'Npitpo = ''A''' + ' And Npiest = '' ''';                                         
        When Operador = '-';                                                                        
         Diferencia = *Zeros;                                                                       
         Diferencia = Pbase - Operacion;                                                            
         Wdes = 'Modificación cupo base por traslado de #' +                                        
                 %Trim(%Editc(Operacion:'J')) + ' ejecutivas a otro ' +                             
                'sector, con actividad ' + %Trim(%Editc(RcbactW:'J')) +                             
                '%, disminuye cupo base de ' + %Trim(%Editc(Pbase:'J'))+                            
                ' a ' + %Trim(%Editc(Diferencia:'J')) + ' pedidos.';                                
         SqlStr = 'Update Votrea00/Vtmnpif Set Npinro = Npinro - ' +                                
                  %Trim(%Editc(Operacion:'Z')) + ' Where ' +                                        
                  'Npicia = ' + '''' + Pcia + '''' + ' And ' +                                      
                  'Npizna = ' + '''' + Wzona + '''' + ' And ' +                                     
                  'Npicdg = ' + %Trim(%Editc(Wcedula:'Z')) + ' And ' +                              
                  'Npisec = ' + '''' + Wsect + '''' + ' And ' +                                     
                  'Npitpo = ''A''' + ' And Npiest = '' ''';                                         
       EndSl;                                                                                       
                                                                                                    
       If %Len(%Trim(SqlStr)) > *Zeros;                                                             
        Tsw = 'S';                                                                                  
        Exec Sql                                                                                    
         Execute Immediate :SqlStr;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Update Votrea00/Vtmnpif Set Npinro = 0                                                     
         Where Npicia = :Pcia And                                                                   
               Npizna = :Wzona And                                                                  
               Npitpo = 'A' And                                                                     
               Npinro < 0;                                                                          
                                                                                                    
        PrHistoria();                                                                               
        PrEnviaCorreo();                                                                            
       Endif;                                                                                       
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrRecalcularCupoBaseVtmnpif...                                                                
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Recalcula el Cupo Base de la Líder Vttepmf     //                       
       //------------------------------------------------------------------//                       
     PPrRecalcularCupoBaseVttepmf...                                                                
     P                 B                                                                            
     DPrRecalcularCupoBaseVttepmf...                                                                
     D                 Pi                                                                           
      /Free                                                                                         
       Exec Sql                                                                                     
        Select Epmnro Into :Pbase                                                                   
        From Votrea00/Vttepmf                                                                       
        Where Epmcia = :Pcia And                                                                    
              Epmzna = :Wzona And                                                                   
              Epmest = ' ' And                                                                      
              Epmcdg = :Wcedula And                                                                 
              Epmsec = :Wsect;                                                                      
                                                                                                    
       If SqlCod <> 100 And SqlCod >= *Zeros;                                                       
       SqlStr = *Blanks;                                                                            
       Eval(h) Operacion = (Nro * (RcbactW/100));                                                   
       Select;                                                                                      
        When Operador = '+';                                                                        
         Diferencia = *Zeros;                                                                       
         Diferencia = Pbase + Operacion;                                                            
         Wdes = 'Modificación cupo base por traslado de #' +                                        
                 %Trim(%Editc(Operacion:'J')) + ' ejecutivas a su ' +                               
                'sector, con actividad ' + %Trim(%Editc(RcbactW:'J')) +                             
                '%, aumento cupo base de ' + %Trim(%Editc(Pbase:'J')) +                             
                ' a ' + %Trim(%Editc(Diferencia:'J')) + ' pedidos.';                                
         SqlStr = 'Update Votrea00/Vttepmf Set Epmnro = Epmnro + ' +                                
                  %Trim(%Editc(Operacion:'Z')) + ' Where ' +                                        
                  'Epmcia = ' + '''' + Pcia + '''' + ' And ' +                                      
                  'Epmzna = ' + '''' + Wzona + '''' + ' And ' +                                     
                  'Epmcdg = ' + %Trim(%Editc(Wcedula:'Z')) + ' And ' +                              
                  'Epmsec = ' + '''' + Wsect + '''' + ' And ' +                                     
                  'Epmest = '' ''';                                                                 
        When Operador = '-';                                                                        
         Diferencia = *Zeros;                                                                       
         Diferencia = Pbase - Operacion;                                                            
         Wdes = 'Modificación cupo base por traslado de #' +                                        
                 %Trim(%Editc(Operacion:'J')) + ' ejecutivas a otro ' +                             
                'sector, con actividad ' + %Trim(%Editc(RcbactW:'J')) +                             
                '%, disminuye cupo base de ' + %Trim(%Editc(Pbase:'J'))+                            
                ' a ' + %Trim(%Editc(Diferencia:'J')) + ' pedidos.';                                
         SqlStr = 'Update Votrea00/Vttepmf Set Epmnro = Epmnro - ' +                                
                  %Trim(%Editc(Operacion:'Z')) + ' Where ' +                                        
                  'Epmcia = ' + '''' + Pcia + '''' + ' And ' +                                      
                  'Epmzna = ' + '''' + Wzona + '''' + ' And ' +                                     
                  'Epmcdg = ' + %Trim(%Editc(Wcedula:'Z')) + ' And ' +                              
                  'Epmsec = ' + '''' + Wsect + '''' + ' And ' +                                     
                  'Epmest = '' ''';                                                                 
       EndSl;                                                                                       
                                                                                                    
       If %Len(%Trim(SqlStr)) > *Zeros;                                                             
        Tsw = 'S';                                                                                  
        Exec Sql                                                                                    
         Execute Immediate :SqlStr;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Update Votrea00/Vttepmf Set Epmnro = 0                                                     
         Where Epmcia = :Pcia And                                                                   
               Epmzna = :Wzona And                                                                  
               Epmest = ' ' And                                                                     
               Epmnro < 0;                                                                          
                                                                                                    
        PrHistoria();                                                                               
        PrEnviaCorreo();                                                                            
       Endif;                                                                                       
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrRecalcularCupoBaseVttepmf...                                                                
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba Historia del Cambio del Cupo Base        //                       
       //------------------------------------------------------------------//                       
     PPrHistoria       B                                                                            
     DPrHistoria       Pi                                                                           
     DUser             s             10a   Inz(*User)                                               
      /Free                                                                                         
       Exec Sql                                                                                     
        Insert Into Votrea00/Vtwhrbf Values(:Pcia, :Wdes, :Wzona, :Wsect,                           
         :Wcedula, Char(Current_Date, Iso), Char(Current_Time), :User);                             
                                                                                                    
      /End-Free                                                                                     
     PPrHistoria       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de Enviar el Correo                      //                       
       //------------------------------------------------------------------//                       
     PPrEnviaCorreo    B                                                                            
     DPrEnviaCorreo    Pi                                                                           
     ** Definiciòn de Variables de Trabajo                                                          
     DWlider           s           1000a   Inz(*Blanks)                                             
     DWregional        s           1000a   Inz(*Blanks)                                             
     DWcomercial       s           1000a   Inz(*Blanks)                                             
     DWcomercial1      s           1000a   Inz(*Blanks)                                             
      /Free                                                                                         
       Clear Pdestinatario;                                                                         
       Wcomercial = PrLider();                                                                      
       Wcomercial1 = PrRegional();                                                                  
       If %Len(%Trim(Wcomercial)) > *Zeros;                                                         
        Wcomercial = %Trim(Wcomercial);                                                             
       Else;                                                                                        
        Wcomercial = *Blanks;                                                                       
       Endif;                                                                                       
       If %Len(%Trim(Wcomercial1)) > *Zeros;                                                        
        Wcomercial = %Trim(Wcomercial) + ',' + %Trim(Wcomercial1);                                  
       Endif;                                                                                       
       Pdestinatario = %ScanRpl(',':'|':Wcomercial);                                                
                                                                                                    
       // Valida que como minimo exista un destinatario                                             
       If Pdestinatario <>  *Blanks;                                                                
                                                                                                    
         // Asigna Respuesta                                                                        
         Pres= *Blanks;                                                                             
                                                                                                    
         // Asigna Remitente                                                                        
         Premitente = 'servicioalcliente@leonisa.com';                                              
                                                                                                    
         // Asigna Descripcion mensaje                                                              
         PRemMostrar = 'Modificación Cupo Base por Traslado Zona ' + Pzona                          
                       + ' Sector ' + %Trim(Wsect);                                                 
                                                                                                    
         // Asigna asunto                                                                           
         Pasunto = %Trim(PRemMostrar);                                                              
                                                                                                    
         // Asigna asunto                                                                           
        Select;                                                                                     
         When Operador = '+';                                                                       
          Pmensaje   = 'Cordial saludo, se ha modificado el cupo ' +                                
                      'de la líder ' + %Trim(PrNombre(Wcedula)) +                                   
                      ' con cédula ' + %Trim(%Editc(Wcedula:'Z')) +                                 
                      ', asociada en el plan de alas al sector ' +                                  
                      %Trim(Wsect) + '<br>Motivo:' +                                                
                     'Presenta traslado de # ' +                                                    
                     %Trim(%Editc(Operacion:'J'))+ ' ejecutivas '+                                  
                     'a su sector, teniendo en cuenta una actividad '+                              
                     %Trim(%Editc(RcbactW:'J')) + '%, se le incremen'+                              
                     'ta a su cupo base actual ' +                                                  
                     %Trim(%Editc(Pbase:'J')) + ' en ' +                                            
                     %Trim(%Editc(Operacion:'J')) + ' pedidos, ' +                                  
                    'siendo su nuevo cupo base #' +                                                 
                    %Trim(%Editc(Diferencia:'J')) +'.<br>'+                                         
                    'El cual aplica a partir de este momento.';                                     
         When Operador = '-';                                                                       
          Pmensaje   = 'Cordial saludo, se ha modificado el cupo ' +                                
                      'de la líder ' + %Trim(PrNombre(Wcedula)) +                                   
                      ' con cédula ' + %Trim(%Editc(Wcedula:'Z')) +                                 
                      ', asociada en el plan de alas al sector ' +                                  
                      %Trim(Wsect) + '<br>Motivo:' +                                                
                     'Presenta traslado de # ' +                                                    
                     %Trim(%Editc(Operacion:'J'))+ ' ejecutivas '+                                  
                     'a otro sector, teniendo en cuenta una actividad '+                            
                     %Trim(%Editc(RcbactW:'J')) + '%, se le disminuye'+                             
                     ' a su cupo base actual ' +                                                    
                     %Trim(%Editc(Pbase:'J')) + ' en ' +                                            
                     %Trim(%Editc(Operacion:'J')) + ' pedidos, ' +                                  
                    'siendo su nuevo cupo base #' +                                                 
                    %Trim(%Editc(Diferencia:'J')) +'.<br>'+                                         
                    'El cual aplica a partir de este momento.';                                     
        EndSl;                                                                                      
                                                                                                    
         PReplyto   = *Blanks;                                                                      
         Pcc        = *Blanks;                                                                      
         Pcco       = *Blanks;                                                                      
         Soapr      = *Blanks;                                                                      
                                                                                                    
          EnviaCorreo(PCia           :                                                              
                     Pres            :                                                              
                     Premitente      :                                                              
                     Pdestinatario   :                                                              
                     PRemMostrar     :                                                              
                     PAsunto         :                                                              
                     Pmensaje        :                                                              
                     PReplyto        :                                                              
                     Pcc             :                                                              
                     Pcco            :                                                              
                     Soapr            );                                                            
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrEnviaCorreo    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Email del Area Comercial Regional  //                       
       //------------------------------------------------------------------//                       
     PPrRegional       B                                                                            
     DPrRegional       Pi          1000a                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DEmail            s            200a   Inz(*Blanks)                                             
     DWcomercial       s           1000a   Inz(*Blanks)                                             
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DNro              s              3  0 Inz(*Zeros)                                              
     DComa             s              1a   Inz(',')                                                 
      /Free                                                                                         
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Divemd ' +                                                                  
                'From Votrea00/Vtmdivf ' +                                                          
                'Where Trim(Divpai) = ' + '''' + Pcia + '''' + ' And ' +                            
                'Trim(Divdiv) = ' + '''' + %Trim(Xreg) + '''' + ' And ' +                           
                'Divest = '' ''';                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur3 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor3 Cursor For Cur3;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor3;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor3 Into :Email;                                                                  
                                                                                                    
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
         WComercial = %Trim(WComercial) + %Trim(Coma) + %Trim(Email);                               
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch Cursor3 Into :Email;                                                                 
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor3;                                                                              
                                                                                                    
       Return WComercial;                                                                           
      /End-Free                                                                                     
     PPrRegional       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Nombre de la Compradora            //                       
       //------------------------------------------------------------------//                       
     PPrNombre         B                                                                            
     DPrNombre         Pi            60a                                                            
     DCedula                          9  0                                                          
     ** Definiciòn de Variables de Trabajo                                                          
     DNombre           s             60a   Inz(*Blanks)                                             
      /Free                                                                                         
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Mlname ' +                                                                  
                'From Opcapf/Pmllst' + Pcia + ' ' +                                                 
                'Where MlcstÑ = ' + %Trim(%Editc(Cedula:'Z')) +                                     
                ' Fetch First 1 Row Only';                                                          
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur4 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor4 Cursor For Cur4;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor4;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor4 Into :Nombre;                                                                 
                                                                                                    
       Return Nombre;                                                                               
      /End-Free                                                                                     
     PPrNombre         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Email del Area Comercial Lider     //                       
       //------------------------------------------------------------------//                       
     PPrLider          B                                                                            
     DPrLider          Pi          1000a                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DEmail            s            200a   Inz(*Blanks)                                             
     DWcomercial       s           1000a   Inz(*Blanks)                                             
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DNro              s              3  0 Inz(*Zeros)                                              
     DComa             s              1a   Inz(',')                                                 
      /Free                                                                                         
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Daleml, Sminf2 ' +                                                          
                'From Vpcopa00/Vtmdalf Inner Join Opf' + Pcia + '/Pslman ' +                        
                'On Dalcpz = ' + '''' + Pcia + '''' + ' And ' +                                     
                'Trim(Dalced) = Trim(Sminf1) ' +                                                    
                'Where Trim(Dalcpz) = ' + '''' + Pcia + '''' + ' And ' +                            
                'Trim(Smslsp) = ' + '''' + %Trim(Pzona) + '''' + ' And ' +                          
                'Smdele = '' ''';                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur5 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor5 Cursor For Cur5;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor5;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor5 Into :Email, :Xreg;                                                           
                                                                                                    
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
         WComercial = %Trim(WComercial) + %Trim(Coma) + %Trim(Email);                               
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch Cursor5 Into :Email, :Xreg;                                                          
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor5;                                                                              
                                                                                                    
       Return WComercial;                                                                           
      /End-Free                                                                                     
     PPrLider          E                                                                            
