      **********************************************************************                        
      ** Programa     : VTMCCC25R                                         **                        
      ** Descripciòn: : Mant. Maestro Correos Usuarios de VOTRE           **                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : John William Palacio PersonalSoft                 **                        
      ** Fecha Creaciòn: 24 de Junio de 2016                              **                        
      **********************************************************************                        
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definiciòn de Archivo de Trabajo                                                            
     FVTMCUVF   Uf A e           k Disk                                                             
     FVTMCUVL1  Uf A e           k Disk      RENAME(RegCuv:Reg00)                                   
                                                                                                    
     FVTMCCC25P Cf   e             Workstn                                                          
     F                                     Sfile(RDatos1:Nrr)                                       
     F                                     InfDs( infDsp )                                          
                                                                                                    
      *Definición de variables de trabajo                                                           
     DPosix            s              4  0 Inz(*Zeros)                                              
     DPtr              s               *   Inz(%Addr(*In))                                          
     Dcia              s              3a                                                            
     DW_User           s             10a   Inz(*User)                                               
     DRespuestaml      S              1A                                                            
     DW_Email          S             80A                                                            
     DW_Tipo           S              1A                                                            
     Dmax              s              5  0 Inz(0)                                                   
     DConta            s              4s 0 Inz(0)                                                   
     DWSql             s           2000                                                             
     D$c               c                   ''''                                                     
     DDcia             S              3a   Inz('001')                                               
     DDTipo            s             10a                                                            
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DListar                           N   Overlay(Indic:4)                                         
     DGrabar                           N   Overlay(Indic:5)                                         
     DCrear                            N   Overlay(Indic:6)                                         
     DConsultar                        N   Overlay(Indic:7)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:70)                                        
                                                                                                    
     D InfDsp          DS                                                                           
     D                                     Qualified                                                
     D   key                          1A   Overlay( InfDsp: 369 )                                   
                                                                                                    
      *Definición de Estructuras de Trabajo                                                         
     DFields         e Ds                  Extname(VTMCUVF:REGCUV)                                  
     D                                     Prefix(Q)                                                
                                                                                                    
     D                sDs                                                                           
     Dnomp               *Proc                                                                      
                                                                                                    
     DListaTipProc     Pr                  Extpgm('VOTREP00/VTMCCC26R')                             
     DDcia                            3a                                                            
     DDTipo                          10a                                                            
                                                                                                    
                                                                                                    
      *Definición de Procedimientos                                                                 
     DPintarfile       Pr                                                                           
     DBorrarfile       Pr                                                                           
     DPrCrear          Pr                                                                           
     DPrValidarCampos  Pr              N                                                            
     DLeerOpcion       Pr                                                                           
     DPrActualizar     Pr                                                                           
     DPrEliminar       Pr                                                                           
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //                                                                  //                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //                                                                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /Free                                                                                         
       Dow Salir = *Off;                                                                            
         If Salir = *On;                                                                            
           Leave;                                                                                   
         EndIf;                                                                                     
                                                                                                    
         If *In12=*on;                                                                              
           Clear RControl1;                                                                         
         EndIf;                                                                                     
         PintarFile();                                                                              
         If SitNom <> *Blanks Or  SitPro <> *Blanks;                                                
           Iter;                                                                                    
           //SituarEn();                                                                            
           //Posix = Conta;                                                                         
         EndIf;                                                                                     
                                                                                                    
         If Posix > *Zeros;                                                                         
           Posic = Posix;                                                                           
           Posix = *Zeros;                                                                          
         Endif;                                                                                     
         Pfecha = %Char(%Date());                                                                   
         Ppgm = Nomp;                                                                               
         SflDspCtl = *On;                                                                           
         Write Teclas1;                                                                             
         Exfmt RControl1;                                                                           
         SflDspCtl = *Off;                                                                          
                                                                                                    
         If Listar = *On;                                                                           
           Clear DTipo;                                                                             
           ListaTipProc(Dcia:DTipo);                                                                
           PFILPRO = DTipo;                                                                         
           Iter;                                                                                    
         EndIf;                                                                                     
                                                                                                    
         If Crear = *On;                                                                            
           PrCrear();                                                                               
         EndIf;                                                                                     
                                                                                                    
         If Nrr > *Zeros;                                                                           
           LeerOpcion();                                                                            
         Endif;                                                                                     
       EndDo;                                                                                       
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //        Procedimiento para pintar el subfile principal            //                        
      //////////////////////////////////////////////////////////////////////                        
     PPintarfile       B                                                                            
     Dpintarfile       Pi                                                                           
      /Free                                                                                         
       Borrarfile();                                                                                
       WSql='Select Upper(CUVUSU), CUVNOM, CUVMAL, CUVENV, CUVPRO '+                                
            'From Votrea00/Vtmcuvf '+                                                               
            'Where 1 = 1  ';                                                                        
            //'Where (Substr(CuvPro, 1, 2) = '+$C+'EC'+$C + ' OR ' +                                
          //'CUVPRO Like ' + '''' + 'PRECIE%' + '''' + ' OR CUVPRO Like ' +                         
          // '''' + 'CIERRE%' + '''' + ')';                                                         
                                                                                                    
       If PFILPRO <> *Blanks;                                                                       
          WSql =%Trim(WSql) + ' And CUVPRO = '+$C+ PFILPRO+$C;                                      
       EndIf;                                                                                       
                                                                                                    
       If SITNOM <> *Blanks;                                                                        
          WSql =%Trim(WSql) + ' And  UPPER(CUVUSU) like '+$C+ '%'+                                  
                              %trim(SITNOM) +'%' + $C;                                              
          Sitnom = ' ';                                                                             
       EndIf;                                                                                       
                                                                                                    
       If SITPRO <> *Blanks AND PFILPRO = *Blanks;                                                  
          WSql =%Trim(WSql) + ' And CUVPRO like '+$C+ '%'+                                          
                              %trim(SITPRO) +'%' + $C;                                              
          SitPro = ' ';                                                                             
       EndIf;                                                                                       
                                                                                                    
                                                                                                    
       WSql =%Trim(WSql) + ' Order By CuvUsu';                                                      
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor1 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
       Declare Cursor1 Scroll Cursor For Cursor1;                                                   
                                                                                                    
       Exec Sql                                                                                     
         Open cursor1;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         Fetch Cursor1 Into :Fields;                                                                
                                                                                                    
       Dow SqlCod = *Zeros;                                                                         
         PUsu = QCuvUsu;                                                                            
         PCor = QCuvMal;                                                                            
         PPro = QCuvPro;                                                                            
         PEnv = QCuvEnv;                                                                            
         Nrr = Nrr + 1;                                                                             
         Posi = nrr;                                                                                
         Write RDatos1;                                                                             
                                                                                                    
         Exec Sql                                                                                   
           Fetch cursor1 into :Fields;                                                              
       EndDo;                                                                                       
       Max = Nrr;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         Close cursor1;                                                                             
                                                                                                    
       SflEnd = *On;                                                                                
       If Nrr > *Zeros;                                                                             
         SflDsp = *On;                                                                              
       Else;                                                                                        
         SflDsp = *Off;                                                                             
       Endif;                                                                                       
      /End-Free                                                                                     
     PPintarfile       E                                                                            
                                                                                                    
                                                                                                    
       //////////////////////////////////////////////////////////////////////                       
       // Procedimiento que se Encarga de Borrar Archivo de Control        //                       
       //////////////////////////////////////////////////////////////////////                       
     PBorrarfile       B                                                                            
     DBorrarfile       Pi                                                                           
      /Free                                                                                         
       Posic = 1;                                                                                   
       Nrr = *Zeros;                                                                                
       SflClr = *On;                                                                                
       Write RControl1;                                                                             
       SflClr = *Off;                                                                               
      /End-Free                                                                                     
     PBorrarfile       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Lee las Opciones de la Pantalla                //                       
       //------------------------------------------------------------------//                       
     PLeerOpcion       B                                                                            
     DLeerOpcion       Pi                                                                           
      /Free                                                                                         
       If Nrr > *Zeros;                                                                             
         Readc Rdatos1;                                                                             
         Dow Not %Eof();                                                                            
           Select;                                                                                  
             When Pop = '2';                                                                        
               PrActualizar();                                                                      
             When Pop = '4';                                                                        
               PrEliminar();                                                                        
             When Pop = '5';                                                                        
               PrConsultar();                                                                       
           Endsl;                                                                                   
           Pop = *Blanks;                                                                           
           Update Rdatos1;                                                                          
          Readc Rdatos1;                                                                            
         Enddo;                                                                                     
       Endif;                                                                                       
      /End-Free                                                                                     
     PLeerOpcion       E                                                                            
                                                                                                    
       //////////////////////////////////////////////////////////////////////                       
       // Procedimiento que se Encarga de crear un registro                //                       
       //////////////////////////////////////////////////////////////////////                       
     PPrCrear          B                                                                            
     DPrCrear          Pi                                                                           
     DTsw              s               n   Inz(*Off)                                                
      /Free                                                                                         
       Clear Rdato1;                                                                                
       W_Tipo = 'C';                                                                                
                                                                                                    
       Dow Salir = *Off;                                                                            
         Ptitul = '    Crear Información  ';                                                        
         ExFmt Rdato1;                                                                              
         If Salir = *On;                                                                            
           leave;                                                                                   
         EndIf;                                                                                     
                                                                                                    
         If Cancelar  = *On;                                                                        
           Clear Rdato1;                                                                            
           Iter;                                                                                    
         EndIf;                                                                                     
                                                                                                    
         If Grabar = *On;                                                                           
           If PrValidarCampo = *Off;                                                                
             Tsw = *Off;                                                                            
             Chain (PUsuin:PProin) Vtmcuvl1;                                                        
             If Not %Found(Vtmcuvl1);                                                               
               CuvUsu = PUsuin;                                                                     
               CuvNom = PNobin;                                                                     
               W_Email = Pcorin1 + Pcorin2;                                                         
               CuvMal = W_Email;                                                                    
               CuvEnv = PEnvin;                                                                     
               CuvPro = PProin;                                                                     
               Write Reg00;                                                                         
               If %Error;                                                                           
                 Tsw = *On;                                                                         
               EndIf;                                                                               
             Else;                                                                                  
               Tsw = *On;                                                                           
             EndIf;                                                                                 
                                                                                                    
             If Tsw = *Off;                                                                         
               PMMSJ='Se insertaron los registros, pulse enter.';                                   
               Exfmt mensaje1;                                                                      
               *In61=*on;                                                                           
               Leave;                                                                               
             Else;                                                                                  
               PMMSJ='No se insertaron los registros.';                                             
               Exfmt mensaje1;                                                                      
               Leave;                                                                               
             Endif;                                                                                 
           EndIf;                                                                                   
         EndIf;                                                                                     
                                                                                                    
         If PrValidarCampo = *Off;                                                                  
             Iter;                                                                                  
         EndIf;                                                                                     
                                                                                                    
       EndDo;                                                                                       
       Salir = *Off;                                                                                
      /End-Free                                                                                     
     PPrCrear          E                                                                            
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento que Valida Campos registro Rdato1.                 /                         
      //------------------------------------------------------------------/                         
     PPrValidarCampo   B                                                                            
     DPrValidarCampo   Pi             1N                                                            
                                                                                                    
      ** Definición de Variables de Trabajo                                                         
     DIndCam           S              1N   Inz(*Off)                                                
                                                                                                    
      /Free                                                                                         
       //Se apagan los indicadores de posición                                                      
       Clear Perror;                                                                                
       *In30 = *Off;                                                                                
       *In31 = *Off;                                                                                
       *In32 = *Off;                                                                                
       *In33 = *Off;                                                                                
       *In34 = *Off;                                                                                
                                                                                                    
       If PUsuIn = *Blanks;                                                                         
         *In30 = *On;                                                                               
         Perror = 'Ingrese el usuario ';                                                            
         IndCam = *On;                                                                              
         Return IndCam;                                                                             
       EndIf;                                                                                       
                                                                                                    
       If W_Tipo = 'C';                                                                             
         Chain (PUsuin:PProin) Vtmcuvl1;                                                            
         If %Found(Vtmcuvl1);                                                                       
           *In30 = *On;                                                                             
           Perror = 'Usuario ya tiene el proceso asignado';                                         
           IndCam = *On;                                                                            
           Return IndCam;                                                                           
         EndIf;                                                                                     
       EndIf;                                                                                       
       If PProin = *Blanks;                                                                         
         *In34 = *On;                                                                               
         Perror = 'Ingrese el proceso';                                                             
         IndCam = *On;                                                                              
         Return IndCam;                                                                             
       EndIf;                                                                                       
                                                                                                    
       If PNobIn = *Blanks;                                                                         
         *In31 = *On;                                                                               
         Perror = 'Ingrese nombre de usuario';                                                      
         IndCam = *On;                                                                              
         Return IndCam;                                                                             
       EndIf;                                                                                       
                                                                                                    
                                                                                                    
       W_Email = PCorIn1 + PCorIn2;                                                                 
       If W_Email = *Blanks;                                                                        
         *In32 = *On;                                                                               
         Perror = 'Ingrese el correo electronico';                                                  
         IndCam = *On;                                                                              
         Return IndCam;                                                                             
       Else;                                                                                        
         ValidarMail();                                                                             
         If respuestaml = '1';                                                                      
           *In32 = *On;                                                                             
           Perror = 'Ingrese un correo electonico valido';                                          
           IndCam = *On;                                                                            
           Return IndCam;                                                                           
         EndIf;                                                                                     
       EndIf;                                                                                       
                                                                                                    
       If PEnvin = *Blanks;                                                                         
         *In33 = *On;                                                                               
         Perror = 'Ingrese S o N';                                                                  
         IndCam = *On;                                                                              
         Return IndCam;                                                                             
       EndIf;                                                                                       
                                                                                                    
       Return IndCam;                                                                               
      /End-Free                                                                                     
     PPrValidarCampo   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que elimina el registro                            //                       
       //------------------------------------------------------------------//                       
     PPrEliminar       B                                                                            
     DPrEliminar       Pi                                                                           
      /Free                                                                                         
       Clear Mensaje2;                                                                              
       Pmmsj2 = 'Esta seguro que desea eliminar';                                                   
       Exfmt mensaje2;                                                                              
       If Pmsn2 = 'S';                                                                              
         Chain (PUsu:PPro) Vtmcuvl1;                                                                
         If %Found(Vtmcuvl1);                                                                       
           Delete Reg00;                                                                            
         EndIf;                                                                                     
       Else;                                                                                        
         Posix = Posi;                                                                              
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPrEliminar       E                                                                            
                                                                                                    
       //////////////////////////////////////////////////////////////////////                       
       // Procedimiento pantalla Consulta Detallada                        //                       
       //////////////////////////////////////////////////////////////////////                       
     PPrActualizar     B                                                                            
     DPrActualizar     Pi                                                                           
      /Free                                                                                         
       W_Tipo = 'A';                                                                                
       Chain (PUsu:PPro) Vtmcuvl1;                                                                  
       If %Found(Vtmcuvl1);                                                                         
         PUsuin = CuvUsu;                                                                           
         PProin = CuvPro;                                                                           
         PNobiN = CuvNom;                                                                           
         PCorin1 = %Subst(CuvMal:1:60);                                                             
         PCorin2 = %Subst(CuvMal:61:20);                                                            
         PEnvin = CuvEnv;                                                                           
                                                                                                    
         Dow Salir = *Off;                                                                          
           Ptitul = '    Modificar Información';                                                    
           Exfmt Rdato2;                                                                            
           If Salir = *On;                                                                          
             Leave;                                                                                 
           EndIf;                                                                                   
                                                                                                    
           If Grabar = *On;                                                                         
             If PrValidarCampo = *Off;                                                              
               CuvNom = PNobiN;                                                                     
               CuvMal = PCorin1 + PCorin2;                                                          
               CuvEnv = PEnvin;                                                                     
               Update Reg00;                                                                        
               leave;                                                                               
             EndIf;                                                                                 
           EndIf;                                                                                   
           If PrValidarCampo = *Off;                                                                
               Iter;                                                                                
           EndIf;                                                                                   
         EndDo;                                                                                     
       EndIf;                                                                                       
       Salir = *Off;                                                                                
      /End-Free                                                                                     
     PPrActualizar     E                                                                            
                                                                                                    
       //////////////////////////////////////////////////////////////////////                       
       // Procedimiento pantalla Consulta Detallada                        //                       
       //////////////////////////////////////////////////////////////////////                       
     PPrConsultar      B                                                                            
     DPrConsultar      Pi                                                                           
      /Free                                                                                         
       Chain (PUsu:PPro) Vtmcuvl1;                                                                  
       If %Found(Vtmcuvl1);                                                                         
         PUsuin = CuvUsu;                                                                           
         PProin = CuvPro;                                                                           
         PNobiN = CuvNom;                                                                           
         PCorin1 = %Subst(CuvMal:1:60);                                                             
         PCorin2 = %Subst(CuvMal:61:20);                                                            
         PEnvin = CuvEnv;                                                                           
                                                                                                    
         Dow Salir = *Off;                                                                          
           Ptitul = '    Consultar Información';                                                    
           Exfmt Rdato3;                                                                            
           If Salir = *On;                                                                          
             Leave;                                                                                 
           EndIf;                                                                                   
         EndDo;                                                                                     
       EndIf;                                                                                       
       Salir = *Off;                                                                                
      /End-Free                                                                                     
     PPrConsultar      E                                                                            
                                                                                                    
     P*--------------------------------------------------                                           
     P* Procedure name: validarmail                                                                 
     P* Purpose:        validar que le mail contenga @ y .                                          
     P* Returns:                                                                                    
     P*--------------------------------------------------                                           
     P validarmail     B                                                                            
     D validarmail     PI                                                                           
     Dcor              s             10  0                                                          
     Dttl              s             10  0                                                          
     Dcor2             s             30                                                             
      /FREE                                                                                         
       Cor = %Scan('@':W_Email);                                                                    
       Ttl = %len(W_Email);                                                                         
       If Cor > *Zeros;                                                                             
         Cor2 = %subst(W_Email:cor:ttl-cor);                                                        
         If %Scan('.':Cor2) > *Zeros;                                                               
           Respuestaml = '0';                                                                       
         Else;                                                                                      
           respuestaml = '1';                                                                       
         EndIf;                                                                                     
       Else;                                                                                        
         Respuestaml = '1';                                                                         
       EndIf;                                                                                       
      /END-FREE                                                                                     
     P validarmail     E                                                                            
                                                                                                    
       //////////////////////////////////////////////////////////////////////                       
       // Procedimiento que se Encarga de situar en la ZONA Indicada      ///                       
       //////////////////////////////////////////////////////////////////////                       
     PSituarEn         B                                                                            
     DSituarEn         Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DOsw              s              1a   Inz(*Blanks)                                             
     DOsw1             s              1a   Inz(*Blanks)                                             
     Dw_contTemp       s              9  0 Inz(*Zeros)                                              
      /Free                                                                                         
       Clear Conta;                                                                                 
       conta += 1;                                                                                  
       Dow Conta <= Max;                                                                            
         Chain Conta RDatos1;                                                                       
         If SitNom <> *Blanks And SitPro <> *Blanks;                                                
           If SitNom = PUSU And SitPro = PPRO;                                                      
             Osw = 'S';                                                                             
             W_contTemp= Conta;                                                                     
             Leave;                                                                                 
           EndIf;                                                                                   
           If %Trim(PUsu) >= %Trim(SitNom) And Osw = *Blanks                                        
              And %trim(PPRO) >= %Trim(SitPro);                                                     
             Osw = 'S';                                                                             
             W_contTemp= Conta;                                                                     
           elseIf %Trim(PUsu) >= %Trim(SitNom) And Osw1 = *Blanks;                                  
             Osw1 = 'S';                                                                            
             W_contTemp= Conta;                                                                     
           Else;                                                                                    
             Conta += 1;                                                                            
           Endif;                                                                                   
         ElseIf SitNom <> *Blanks And SitPro = *Blanks;                                             
           If %Trim(PUsu) = %Trim(SitNom);                                                          
             Osw = 'S';                                                                             
             SitNom = *Blanks;                                                                      
             SitPro = *Blanks;                                                                      
             W_contTemp= Conta;                                                                     
             Leave;                                                                                 
           elseIf %Trim(PUsu) >= %Trim(SitNom) And Osw1 = *Blanks;                                  
             Osw1 = 'S';                                                                            
             W_contTemp= Conta;                                                                     
           Else;                                                                                    
             Conta += 1;                                                                            
           Endif;                                                                                   
         ElseIf SitNom = *Blanks And SitPro <> *Blanks;                                             
           If SitPro = PPRO;                                                                        
             Osw = 'S';                                                                             
             W_contTemp= Conta;                                                                     
             Leave;                                                                                 
           EndIf;                                                                                   
           If %Trim(PPro) >= %Trim(SitPro) And Osw = *Blanks;                                       
             Osw = 'S';                                                                             
             W_contTemp= Conta;                                                                     
           Else;                                                                                    
             Conta += 1;                                                                            
           Endif;                                                                                   
         EndIf;                                                                                     
       Enddo;                                                                                       
       If Osw = 'S';                                                                                
          Conta = W_contTemp;                                                                       
          SitNom = *Blanks;                                                                         
          SitPro = *Blanks;                                                                         
       ElseIf Osw1= 'S';                                                                            
          Conta = W_contTemp;                                                                       
          SitNom = *Blanks;                                                                         
          SitPro = *Blanks;                                                                         
       Else;                                                                                        
          Conta = 1;                                                                                
          SitNom = *Blanks;                                                                         
          SitPro = *Blanks;                                                                         
       EndIf;                                                                                       
      /End-Free                                                                                     
     PSituarEn         E                                                                            
                                                                                                    
