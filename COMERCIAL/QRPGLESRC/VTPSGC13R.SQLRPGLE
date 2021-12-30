      ***********************************************************************                       
      ** Programa      : VTPSGC13R                                         **                       
      ** Descripciòn   : Verifica los Sectores de las Líderes              **                       
      ** Proyecto      : Plan de Guías                                     **                       
      ** Autor         : (JOrtega)                                         **                       
      ** Fecha Creación: 21 de Octubre de 2019                             **                       
      ***********************************************************************                       
      ** Definición de Directivas de Compilación                                                    
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
      ** Definición de Variables de Trabajo                                                         
     DKnro             s              9  0 Inz(*Zeros)                                              
     DgCia             s              3a   Inz(*Blanks)                                             
     DgCedula          s              9  0 Inz(*Zeros)                                              
     DgSector          s              1a   Inz(*Blanks)                                             
     DgZona            s              3a   Inz(*Blanks)                                             
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       PrVtmtpgf();                                                                                 
       PrVtmgldf();                                                                                 
       *InLr = *On;                                                                                 
       Return;                                                                                      
      /End-Free                                                                                     
                                                                                                    
      *------------------------------------------------------------------------                     
      * Verifica Lideres Inacvitas por Tipo de Plan Vtmtpgf                                         
      *------------------------------------------------------------------------                     
     PPrVtmtpgf        B                                                                            
     DPrVtmtpgf        Pi                                                                           
      /Free                                                                                         
       Exec Sql                                                                                     
        Declare Cur0 Cursor For                                                                     
        Select Tpgcia, Tpgcdg, Tpgsct, Tpgzna                                                       
        From Votrea00/Vtmtpgf                                                                       
        Where Tpgest = ' '                                                                          
        Order By Tpgcia, Tpgcdg, Tpgzna, Tpgsct                                                     
        For Update Of Tpgest;                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Open Cur0;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur0 Into :gCia, :gCedula, :gSector, :gZona;                                          
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= 0;                                                           
        Clear Knro;                                                                                 
        Exec Sql                                                                                    
         Select Count(0) Into :Knro                                                                 
         From Votrea00/Vtmgldf                                                                      
         Where Gldest = 'D' And                                                                     
               Gldcia = :gCia And                                                                   
               Gldcte = :gCedula And                                                                
               Gldzon = :gZona And                                                                  
               Gldsct = :gSector;                                                                   
         If Knro > 0;                                                                               
          Exec Sql                                                                                  
           Update Votrea00/Vtmtpgf Set Tpgest = 'D'                                                 
           Where Current Of Cur0;                                                                   
         Endif;                                                                                     
        Exec Sql                                                                                    
         Fetch Cur0 Into :gCia, :gCedula, :gSector, :gZona;                                         
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cur0;                                                                                 
      /End-Free                                                                                     
     PPrVtmtpgf        E                                                                            
                                                                                                    
      *------------------------------------------------------------------------                     
      * Verifica Lideres Inacvitas por Tipo de Plan Vtmgldf                                         
      *------------------------------------------------------------------------                     
     PPrVtmgldf        B                                                                            
     DPrVtmgldf        Pi                                                                           
      /Free                                                                                         
       Exec Sql                                                                                     
        Declare Cur1 Cursor For                                                                     
        Select Gldcia, Gldcte, Gldsct                                                               
        From Votrea00/Vtmgldf                                                                       
        Where Gldest = ' ' And                                                                      
              Gldcte In (Select Tpgcdg                                                              
                         From Votrea00/Vtmtpgf                                                      
                         Where Tpgest = ' ' And                                                     
                               Tpgcia = Gldcia And                                                  
                               Tpgcdg = Gldcte And                                                  
                               Tpgsct <> Gldsct)                                                    
        Order By Gldcia, Gldcte;                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Open Cur1;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur1 Into :gCia, :gCedula, :gSector;                                                  
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= 0;                                                           
        Exec Sql                                                                                    
         Update Votrea00/Vtmtpgf Set Tpgsct = :gSector                                              
         Where Tpgest = ' ' And                                                                     
               Tpgcia = :gCia And                                                                   
               Tpgcdg = :gCedula;                                                                   
        Exec Sql                                                                                    
         Fetch Cur1 Into :gCia, :gCedula, :gSector;                                                 
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cur1;                                                                                 
      /End-Free                                                                                     
     PPrVtmgldf        E                                                                            
