*********************************************************************************                   
     ** Programa      : VTPSGBZR                                                *                   
     ** Descripción:  : Envio de Email Pagos Autorizados a Guías Area Comercial *                   
     ** Proyecto      : Plan Guías                                              *                   
     ** Autor         : (JOrtega)                                               *                   
     ** Fecha Creación: 15 de Diciembre de 2016                                 *                   
     ****************************************************************************                   
                                                                                                    
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definición de Variables de Trabajo                                                          
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DIzona            s              3a   Inz(*Blanks)                                             
     DIcaso            s              7a   Inz(*Blanks)                                             
     DIcedula          s             30a   Inz(*Blanks)                                             
     DIfecha           s             10a   Inz(*Blanks)                                             
     DOrespuesta       s              1a   Inz(*Blanks)                                             
     DDngzng           s              3a   Inz(*Blanks)                                             
     DDngscg           s              3a   Inz(*Blanks)                                             
     DDngcdg           s              9  0 Inz(*Zeros)                                              
     DDngmot           s            500    Inz(*Blanks)                                             
     DRegion           s             10a   Inz(*Blanks)                                             
     DRespuesta        s              8a   Inz(*Blanks)                                             
     Dw_Email          s            200a   Inz(*Blanks)                                             
     DPcia             s              3a   Inz(*Blanks)                                             
     DPres             s              2a   Inz(*Blanks)                                             
     DPremitente       s            200a   Inz(*Blanks)                                             
     DPdestinatario    s            200a   Inz(*Blanks)                                             
     DPRemMostrar      s           1000a   Inz(*Blanks)                                             
     DPAsunto          s           1000a   Inz(*Blanks)                                             
     DPmensaje         s          32767a   Inz(*Blanks)                                             
     DPReplyto         s          10000a   Inz(*Blanks)                                             
     DPcc              s          10000a   Inz(*Blanks)                                             
     DPcco             s          10000a   Inz(*Blanks)                                             
     DSoapr            s          32767a   Inz(*Blanks)                                             
     DW_Tabla          s          20000a   Inz(*Blanks)                                             
     DW_TablaD         s          20000a   Inz(*Blanks)                                             
     DTcnsAUX          s              9a   Inz(*Blanks)                                             
     DOsw              s              1a   Inz('N')                                                 
     DPCamE            s              4a   Inz(*Blanks)                                             
     DPCamR            s              4a   Inz(*Blanks)                                             
     DPOper            s              1a   Inz(*Blanks)                                             
     DPnume            s              3a   Inz(*Blanks)                                             
     DP_CamAnt         s              4a   Inz(*Blanks)                                             
                                                                                                    
     ** Definición de Prototipos de Trabajo                                                         
     DPrVttdngf        Pr                                                                           
     DPrVttpprf        Pr                                                                           
     DPrEnviaCorreo    Pr                                                                           
     DPrNombreGuia     Pr            30                                                             
     DPrLider          Pr          1000a                                                            
     DPrRegional       Pr          1000a                                                            
     DPrComercial      Pr          1000a                                                            
     DPrArmarTabla     Pr                                                                           
     DPrArmardetalleT  Pr                                                                           
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pr                  Extpgm('VTPSGBYR')                                       
     DP_Cia                           3a                                                            
     DP_Zon                           3a                                                            
     DP_Cam                           4a                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DP_Cia                           3a                                                            
     DP_Zon                           3a                                                            
     DP_Cam                           4a                                                            
                                                                                                    
     DPgmNewPlanG      Pr                  Extpgm('VOTREP00/VTMPGB0R')                              
     DPcia                            3a                                                            
     DIzona                           3a                                                            
     DIcaso                           7a                                                            
     DIcedula                        30a                                                            
     DIfecha                         10a                                                            
     DOrespuesta                      1a                                                            
                                                                                                    
     DEnviaCorreo      Pr                  Extpgm('VOTREP00/VTEPWB6R')                              
     DPcia                            3A                                                            
     DPres                            2A                                                            
     DPremitente                    200A                                                            
     DPdestinatario                 200A                                                            
     DPRemMostrar                  1000A                                                            
     DPAsunto                      1000A                                                            
     DPmensaje                    32767A                                                            
     DPReplyto                    10000A                                                            
     DPcc                         10000A                                                            
     DPcco                        10000A                                                            
     DSoapr                       32767A                                                            
                                                                                                    
     DValidarEmail     Pr                  Extpgm('VOTREP00/VTMGOBTR')                              
     DPrtStatus                       8a                                                            
     DPemail                        200a                                                            
                                                                                                    
     DConversion       Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DYcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPCia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       Icedula = *Blanks;                                                                           
       PgmNewPlanG(P_Cia:P_Zon:Icaso:Icedula:Ifecha:Orespuesta);                                    
       If Orespuesta = 'S';                                                                         
        PCamE = P_Cam;                                                                              
        PCamR = *Blanks;                                                                            
        POper = '-';                                                                                
        Pnume = '1';                                                                                
        ProCampa(P_cia:PCamE:PCamR:POper:Pnume);                                                    
        P_CamAnt = %Trim(PCamR);                                                                    
        PrVttdngf();                                                                                
        PrVttpprf();                                                                                
       Endif;                                                                                       
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Verifica que Pagos de Guías Estan Pendientes   //                       
       // por Aplicar por que no Cumplieron las Condiciones pero Fueron    //                       
       // Aprobados por el Area Comercial                                  //                       
       //------------------------------------------------------------------//                       
     PPrVttdngf        B                                                                            
     DPrVttdngf        Pi                                                                           
      /Free                                                                                         
       Exec sql                                                                                     
        Declare Cur0 Cursor For                                                                     
        Select Distinct Dngzng, Dngscg, Dngcdg, Dngmot                                              
        From Votrea00/Vttdngf                                                                       
        Where Dngcia = :P_Cia And                                                                   
              Dngzng = :P_Zon And                                                                   
              Dngcmp = :P_CamAnt And                                                                
              Dngvlr > 0 And (Dngest = 'A' Or Dngest = 'E'                  )                       
        //    Dngaut <> ' ' And Dngvlr > 0 And (Dngest = 'A' Or Dngest = 'E')                       
        //    And Dngaut <> 'Se Autoriza el Pago a la Guía ya que Envío el RUT.'                    
              And Dngfec = Char(Current Date, Iso)                                                  
        Order By Dngcdg Asc;                                                                        
                                                                                                    
       Exec sql                                                                                     
        Open Cur0;                                                                                  
                                                                                                    
       Exec sql                                                                                     
        Fetch Cur0 Into :Dngzng, :Dngscg, :Dngcdg, :Dngmot;                                         
                                                                                                    
       TcnsAUX = *Blanks;                                                                           
       Osw = 'N';                                                                                   
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        If Osw = 'N';                                                                               
         Osw = 'S';                                                                                 
         PrArmarTabla();                                                                            
        Endif;                                                                                      
        PrArmardetalleT();                                                                          
        Exec sql                                                                                    
         Fetch Cur0 Into :Dngzng, :Dngscg, :Dngcdg, :Dngmot;                                        
       Enddo;                                                                                       
                                                                                                    
       Exec sql                                                                                     
        Close Cur0;                                                                                 
                                                                                                    
       If Osw = 'S';                                                                                
        PrEnviaCorreo();                                                                            
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrVttdngf        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Verifica que Pagos de Guías Estan Pendientes   //                       
       // por Aplicar por que no Cumplieron las Condiciones del Envio del  //                       
       // RUT                                                              //                       
       //------------------------------------------------------------------//                       
     PPrVttpprf        B                                                                            
     DPrVttpprf        Pi                                                                           
      /Free                                                                                         
       Exec sql                                                                                     
        Declare Cur0r Cursor For                                                                    
        Select Distinct Pprzna, Pprsec, Pprcdg, Pprmot                                              
        From Votrea00/Vttpprf                                                                       
        Where Pprcia = :P_Cia And                                                                   
              Pprzna = :P_Zon And                                                                   
              Pprcmp = :P_CamAnt And                                                                
              Ppraut <> ' ' And Pprvlr > 0 And Pprest = 'A'                                         
         //   And Ppraut <> 'Se Autoriza el Pago a la Guía ya que Envío el RUT.'                    
              And Pprfec = Char(Current Date, Iso)                                                  
        Order By Pprcdg Asc;                                                                        
                                                                                                    
       Exec sql                                                                                     
        Open Cur0r;                                                                                 
                                                                                                    
       Exec sql                                                                                     
        Fetch Cur0r Into :Dngzng, :Dngscg, :Dngcdg, :Dngmot;                                        
                                                                                                    
       TcnsAUX = *Blanks;                                                                           
       Osw = 'N';                                                                                   
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        If Osw = 'N';                                                                               
         Osw = 'S';                                                                                 
         PrArmarTabla();                                                                            
        Endif;                                                                                      
        PrArmardetalleT();                                                                          
        Exec sql                                                                                    
         Fetch Cur0r Into :Dngzng, :Dngscg, :Dngcdg, :Dngmot;                                       
       Enddo;                                                                                       
                                                                                                    
       Exec sql                                                                                     
        Close Cur0r;                                                                                
                                                                                                    
       If Osw = 'S';                                                                                
        PrEnviaCorreo();                                                                            
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrVttpprf        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Busca el Nombre de la Guía                     //                       
       //------------------------------------------------------------------//                       
     PPrNombreGuia     B                                                                            
     DPrNombreGuia     Pi            30                                                             
     ** Definición de Variables de Trabajo                                                          
     DWnombre          s             30a   Inz(*Blanks)                                             
      /Free                                                                                         
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Mlname ' +                                                                  
                'From Opf' + P_cia + '/Lmllst01 ' +                                                 
                'Where MlcstÑ = ' + %Trim(%Editc(Dngcdg:'Z'));                                      
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur From :SqlStr;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor Cursor For Cur;                                                              
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor;                                                                                
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor Into :Wnombre;                                                                 
                                                                                                    
       If Sqlcod = 100 Or SqlCod < *Zeros;                                                          
        Wnombre = *Blanks;                                                                          
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor;                                                                               
                                                                                                    
       Return Wnombre;                                                                              
      /End-Free                                                                                     
     PPrNombreGuia     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de Enviar el Correo                      //                       
       //------------------------------------------------------------------//                       
     PPrEnviaCorreo    B                                                                            
     DPrEnviaCorreo    Pi                                                                           
     ** Definiciòn de Variables de Trabajo                                                          
     DWlider           s           1000a   Inz(*Blanks)                                             
     DWregional        s           1000a   Inz(*Blanks)                                             
     DWcomercial       s           1000a   Inz(*Blanks)                                             
      /Free                                                                                         
       Clear Pdestinatario;                                                                         
       Wlider = PrLider();                                                                          
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
                                                                                                    
       // Valida que como minimo exista un destinatario                                             
       If Pdestinatario <>  *Blanks;                                                                
                                                                                                    
         // Asigna Respuesta                                                                        
         Pres= *Blanks;                                                                             
                                                                                                    
         // Asigna Remitente                                                                        
         Premitente = 'servicioalcliente@leonisa.com';                                              
                                                                                                    
         // Asigna Descripcion mensaje                                                              
         PRemMostrar = 'Descuentos Autorizados por Condiciones Zona(' +                             
                       P_Zon + ')';                                                                 
                                                                                                    
         // Asigna asunto                                                                           
         Pasunto = %Trim(PRemMostrar);                                                              
                                                                                                    
         // Asigna asunto                                                                           
         Pmensaje   = 'Querida Líder, <br>' +                                                       
                      'El descuento de la Campaña ' + %Subst(P_CamAnt:3:2) +                        
                      ' que no le fue aplicado a las guías relacionadas a ' +                       
                      'continuación, debido a que no cumplieron las ' +                             
                      '<b>Condiciones del Plan, fueron autorizados </b><br> ' +                     
                      %Trim(W_Tabla) + ' ' + %Trim(W_TablaD) + '</table>';                          
                                                                                                    
         PReplyto   = *Blanks;                                                                      
         Pcc        = *Blanks;                                                                      
         Pcco       = *Blanks;                                                                      
         Soapr      = *Blanks;                                                                      
                                                                                                    
          EnviaCorreo(P_Cia          :                                                              
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
       // Procedimiento que Consulta el Email de la Líder                  //                       
       //------------------------------------------------------------------//                       
     PPrLider          B                                                                            
     DPrLider          Pi          1000a                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DEmail            s            200a   Inz(*Blanks)                                             
     DWlider           s           1000a   Inz(*Blanks)                                             
      /Free                                                                                         
                                                                                                    
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Daleml, Sminf2 ' +                                                          
                'From Vpcopa00/Vtmdalf Inner Join Opf' + P_cia +                                    
                '/Pslman On Trim(Sminf1) = Trim(Dalced) ' +                                         
                'Where Trim(Smslsp) = ' + '''' + P_Zon + '''' + ' And ' +                           
                'Smdele = '' ''' + ' ' +                                                            
                'Fetch First 1 Row Only';                                                           
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur1 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1 Cursor For Cur1;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1 Into :Email, :Region;                                                         
                                                                                                    
       If Sqlcod = 100 Or Sqlcod < *Zeros;                                                          
        Email = *Blanks;                                                                            
        Region = *Blanks;                                                                           
       Endif;                                                                                       
                                                                                                    
       Respuesta = *Blanks;                                                                         
       w_Email = Email;                                                                             
       ValidarEmail(Respuesta:w_Email);                                                             
       If %Trim(Respuesta) = 'Invalido';                                                            
        Wlider = *Blanks;                                                                           
       Else;                                                                                        
        Wlider = Email;                                                                             
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor1;                                                                              
                                                                                                    
       Return Wlider;                                                                               
      /End-Free                                                                                     
     PPrLider          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Email del Regional                 //                       
       //------------------------------------------------------------------//                       
     PPrRegional       B                                                                            
     DPrRegional       Pi          1000a                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DEmail            s            200a   Inz(*Blanks)                                             
     DWregional        s           1000a   Inz(*Blanks)                                             
     DSqlStr           s           1000a   Inz(*Blanks)                                             
      /Free                                                                                         
                                                                                                    
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Divemd ' +                                                                  
                'From Votrea00/Vtmdivf ' +                                                          
                'Where Trim(Divpai) = ' + '''' + P_cia + '''' + ' And ' +                           
                'Divest = '' ''' + ' And Divdiv =' + '''' + %Trim(Region)                           
                + ''' ' + ' ' +                                                                     
                'Fetch First 1 Row Only';                                                           
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur2 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor2 Cursor For Cur2;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor2;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor2 Into :Email;                                                                  
                                                                                                    
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
        Close Cursor2;                                                                              
                                                                                                    
       Return Wregional;                                                                            
      /End-Free                                                                                     
     PPrRegional       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Email del Area Comercial           //                       
       //------------------------------------------------------------------//                       
     PPrComercial      B                                                                            
     DPrComercial      Pi          1000a                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DEmail            s            200a   Inz(*Blanks)                                             
     DWcomercial       s           1000a   Inz(*Blanks)                                             
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DNro              s              3  0 Inz(*Zeros)                                              
     DComa             s              1a   Inz(',')                                                 
      /Free                                                                                         
                                                                                                    
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Seceml ' +                                                                  
                'From Votrea00/Vtmsecf ' +                                                          
                'Where Trim(Seccia) = ' + '''' + P_cia + '''' + ' And ' +                           
                'Trim(Secdiv) = ' + '''' + %Trim(Region) + '''' + ' And ' +                         
                'Secest = '' ''';                                                                   
                                                                                                    
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
         WComercial = %Trim(WComercial) + %Trim(Email) + %Trim(Coma);                               
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch Cursor3 Into :Email;                                                                 
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor3;                                                                              
                                                                                                    
       Return WComercial;                                                                           
      /End-Free                                                                                     
     PPrComercial      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de Armar el Encabezado del Correo        //                       
       //------------------------------------------------------------------//                       
     PPrarmarTabla     B                                                                            
     DPrArmarTabla     Pi                                                                           
      /Free                                                                                         
       W_Tabla = '<table border=' + '''' + '1' + '''' +                                             
                 ' cellpadding=' + '''' + '0' + '''' +                                              
                 ' cellspacing=' + '''' + '0' + '''' +                                              
                 'width=' + '''' + '100%' + '''' + '> <tbody> ' +                                   
                 '<tr style=' + '''' + 'background-color'+                                          
                 ':#1f3251; color:white;'+                                                          
                 'text-align:center; height: 25px;' + '''' + '>' +                                  
                 '<td>Sector</td>' +                                                                
                 '<td>Cedula</td> '+                                                                
                 '<td>Nombre</td>' +                                                                
                 '<td>Causales</td></tr>';                                                          
      /End-Free                                                                                     
     PPrArmarTabla     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de Armar el Detalle del Correo           //                       
       //------------------------------------------------------------------//                       
     PPrArmarDetalleT  B                                                                            
     DPrArmardetalleT  Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DTced             s             30a   Inz(*Blanks)                                             
     DTcns             s              9a   Inz(*Blanks)                                             
      /Free                                                                                         
       Dngmot = %ScanRpl('$':'\\$':Dngmot);                                                         
       Tced = *Blanks;                                                                              
       Tcns = %Trim(%Editc(Dngcdg:'Z'));                                                            
       Conversion(P_Cia:Tced:Tcns);                                                                 
       If TcnsAUX <> Tcns;                                                                          
        TcnsAUX = Tcns;                                                                             
        W_TablaD = %Trim(W_TablaD) +                                                                
       '<tr style=' + '''' + 'text-align:center;height' +                                           
       ': 25px;' + '''' + '>'+                                                                      
                  '<td>' + %Trim(Dngscg) + '</td>' +                                                
                  '<td>' + %Trim(Tced) + '</td>' +                                                  
                  '<td>' + %Trim(PrNombreGuia()) + '</td>' +                                        
                  '<td>' + %Trim(Dngmot) + '</td></tr>';                                            
       Else;                                                                                        
        W_TablaD = %Trim(W_TablaD) +                                                                
       '<tr style=' + '''' + 'text-align:center;height' +                                           
       ': 25px;' + '''' + '>'+                                                                      
                  '<td>' + ' ' + '</td>' +                                                          
                  '<td>' + ' ' + '</td>' +                                                          
                  '<td>' + ' ' + '</td>' +                                                          
                  '<td>' + %Trim(Dngmot) + '</td></tr>';                                            
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrArmardetalleT  E                                                                            
