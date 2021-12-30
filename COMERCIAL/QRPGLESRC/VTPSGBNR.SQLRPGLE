***************************************************************************                         
     ** Programa      : VTPSGBNR                                          *                         
     ** Descripción:  : PGM Que Valida las Políticas Comerciales por Guía *                         
     **                 y determine si se puede realizar el pago o no por *                         
     **                 Campaña                                           *                         
     ** Proyecto      : Nuevo Plan de Guías                               *                         
     ** Autor         : John William Palacio PersonalSoft                 *                         
     ** Fecha Creación: 29 de Junio de 2016                               *                         
     **********************************************************************                         
                                                                                                    
      ** Definición de Directivas de Compilación                                                    
       Ctl-Opt Dftactgrp(*no) Bnddir('QC2LE');                                                      
                                                                                                    
      ** Definición de archivos de Trabajo                                                          
       Dcl-f Vtwpcgf Disk Usage(*Input) Keyed;                                                      
       Dcl-f Vttdngf Disk Usage(*Output) Keyed;                                                     
                                                                                                    
      ** Definición de Variables de Trabajo                                                         
       Dcl-s Wzona Char(3) Inz(*Blanks);                                                            
       Dcl-s W_Cedula Packed(9:0) Inz(*Zeros);                                                      
       Dcl-s W_Nro PacKed(4:0) Inz(*Zeros);                                                         
       Dcl-s W_Region Char(10) Inz(*Blanks);                                                        
       Dcl-s W_Cedul Char(10) Inz(*Blanks);                                                         
       Dcl-s W_Omail Char(10) Inz(*Blanks);                                                         
       Dcl-s W_Valor Packed(11:2) Inz(*Zeros);                                                      
       Dcl-s W_Usr Char (10) Inz(*User);                                                            
                                                                                                    
      ** Definición de Procedimientos                                                               
       Dcl-Pr PrVttdngf End-Pr;                                                                     
                                                                                                    
      ** Definición de Pgm Externos                                                                 
       Dcl-Pr PgmRegion Extpgm('VOTREP00/VTMGOIGR');                                                
         Xcia Char(3);                                                                              
         Xzona Char(3);                                                                             
         Xregion Char(10);                                                                          
         Xcedula Char(10);                                                                          
         XmailPlan Char(10);                                                                        
       End-Pr;                                                                                      
                                                                                                    
      ** Definición de estructuras                                                                  
       Dcl-Ds Fields ExtName('VTWPCGF') prefix(Q) end-ds;                                           
                                                                                                    
      ** Definición de Procedimientos Parametros de Entrada                                         
       Dcl-Pr Main Extpgm('VTPSGBNR');                                                              
        PCia Char(3);                                                                               
        PCmpAct Char(4);                                                                            
        PCmpAnt Char(4);                                                                            
        Pzona Char(3);                                                                              
        Pcedula Char(9);                                                                            
        PSector Char(3);                                                                            
       End-Pr;                                                                                      
       Dcl-PI Main;                                                                                 
        PCia Char(3);                                                                               
        PCmpAct Char(4);                                                                            
        PCmpAnt Char(4);                                                                            
        Pzona Char(3);                                                                              
        Pcedula Char(9);                                                                            
        PSector Char(3);                                                                            
       End-PI;                                                                                      
                                                                                                    
       //------------------------------------------------------------------/                        
       // Inicio Programa Principal                                        /                        
       //------------------------------------------------------------------/                        
        Clear W_Nro;                                                                                
        W_Cedula = %Int(Pcedula);                                                                   
        //Se valida si hay alguna policita que no cumple                                            
        Exec Sql                                                                                    
          Select Count(0) Into :W_Nro                                                               
          From Votrea00/Vtwpcgf                                                                     
          Where PcgCia = :PCia And                                                                  
                PcgCdg = :W_cedula And                                                              
                //PcgZna = :PZona And                                                               
                //PcgSct = :PSector And                                                             
                PcgCmp = :PCmpAnt And                                                               
                PcgInd = 'N';                                                                       
                                                                                                    
        //Si no cumplio alguna politica impuesta por el area se actualiza                           
        If W_Nro > *Zeros;                                                                          
          Exec Sql                                                                                  
            Update Votrea00/Vtmspgf                                                                 
              Set SpgInp = 'E'                                                                      
            Where SpgCia = :PCia And                                                                
                  SpgZon = :PZona And                                                               
                  SpgCam = :PCmpAnt And                                                             
                  SpgSec = :PSector And                                                             
                  SpgTrr = ' ' And                                                                  
                  SpgCed = :W_cedula;                                                               
        //Se invoca el procedimiento encargado de grabar la información de cada                     
        //Uno de los motivos de No pago                                                             
          PrVttdngf();                                                                              
        EndIf;                                                                                      
        *Inlr = *On;                                                                                
        Return;                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que graba la informacion de cada motivo de NO pago //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrVttdngf;                                                                          
        Exec Sql                                                                                    
          Declare Cursor1 Cursor For                                                                
          Select *                                                                                  
          From Votrea00/Vtwpcgf                                                                     
          Where PcgCia = :PCia And                                                                  
                PcgCdg = :W_cedula And                                                              
                //PcgZna = :PZona And                                                               
                //PcgSct = :PSector And                                                             
                PcgCmp = :PCmpAnt And                                                               
                PcgInd = 'N';                                                                       
                                                                                                    
        Exec Sql                                                                                    
          Open cursor1;                                                                             
                                                                                                    
        Exec Sql                                                                                    
          Fetch Cursor1 Into :Fields;                                                               
                                                                                                    
        Dow sqlcod = *Zeros;                                                                        
          DngEst = 'E';                                                                             
          DngCia = PCia;                                                                            
          CallP(E) PgmRegion(PCia:PZona:W_Region:W_Cedul:W_Omail);                                  
          DngReg = W_Region;                                                                        
          DngZng = Pzona;                                                                           
          DngScg = PSector;                                                                         
          DngCdg = W_Cedula;                                                                        
          DngCmp = PCmpAct;                                                                         
          Clear W_Valor;                                                                            
                                                                                                    
          //Se calcula el valor                                                                     
          Exec Sql                                                                                  
            Select Sum(SpgVps) Into :W_Valor                                                        
            From Votrea00/Vtmspgf                                                                   
            Where SpgCia = :PCia And                                                                
                  SpgZon = :PZona And                                                               
                  SpgCam = :PCmpAnt And                                                             
                  SpgSec = :PSector And                                                             
                  SpgTrr = ' ' And                                                                  
                  SpgCed = :W_Cedula;                                                               
                                                                                                    
          DngVlr = W_Valor;                                                                         
          DngCdr = QPcgCdp;                                                                         
          DngMot = QPcgDes;                                                                         
          DngAut = ' ';                                                                             
          DngFec = %Char(%Date);                                                                    
          DngHor = %Char(%Time);                                                                    
          DngUsr = W_Usr;                                                                           
          Monitor;                                                                                  
          If DngMot <> ' ' And QPcgInd = 'N';                                                       
           Write RegDng;                                                                            
          Endif;                                                                                    
          On-Error;                                                                                 
          Endmon;                                                                                   
                                                                                                    
          Exec Sql                                                                                  
            Fetch Cursor1 Into :Fields;                                                             
        EndDo;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
          Close cursor1;                                                                            
       End-Proc;                                                                                    
                                                                                                    
