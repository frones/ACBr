{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }
{                                       Isaque Pinheiro                        }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrEFDBloco_G_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrEFDBloco_G, ACBrEFDBlocos,
     ACBrEFDBloco_0_Class;

type
  /// TBLOCO_H -
  TBloco_G = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroG001: TRegistroG001;      /// BLOCO G - RegistroG001
    FRegistroG990: TRegistroG990;      /// BLOCO G - RegistroG990

    FRegistroG110Count: Integer;
    FRegistroG125Count: Integer;
    FRegistroG130Count: Integer;
    FRegistroG140Count: Integer;
    FRegistroG126Count: Integer;

    procedure WriteRegistroG110(RegG001: TRegistroG001);
    procedure WriteRegistroG125(RegG110: TRegistroG110);
    procedure WriteRegistroG126(RegG125: TRegistroG125);
    procedure WriteRegistroG130(RegG125: TRegistroG125);
    procedure WriteRegistroG140(RegG130: TRegistroG130);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    function RegistroG001New: TRegistroG001;
    function RegistroG110New: TRegistroG110;
    function RegistroG125New: TRegistroG125;
    function RegistroG126New: TRegistroG126;
    function RegistroG130New: TRegistroG130;
    function RegistroG140New: TRegistroG140;

    procedure WriteRegistroG001;
    procedure WriteRegistroG990;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroG001: TRegistroG001 read FRegistroG001 write FRegistroG001;
    property RegistroG990: TRegistroG990 read FRegistroG990 write FRegistroG990;

    property RegistroG110Count: Integer read FRegistroG110Count write FRegistroG110Count;
    property RegistroG125Count: Integer read FRegistroG125Count write FRegistroG125Count;
    property RegistroG126Count: Integer read FRegistroG126Count write FRegistroG126Count;
    property RegistroG130Count: Integer read FRegistroG130Count write FRegistroG130Count;
    property RegistroG140Count: Integer read FRegistroG140Count write FRegistroG140Count;

  end;

implementation

{ TBloco_G }

constructor TBloco_G.Create;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_G.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_G.CriaRegistros;
begin
  FRegistroG001 := TRegistroG001.Create;
  FRegistroG990 := TRegistroG990.Create;

  FRegistroG110Count := 0;
  FRegistroG125Count := 0;
  FRegistroG126Count := 0;
  FRegistroG130Count := 0;
  FRegistroG140Count := 0;

  FRegistroG990.QTD_LIN_G := 0;
end;

procedure TBloco_G.LiberaRegistros;
begin
  FRegistroG001.Free;
  FRegistroG990.Free;
end;

procedure TBloco_G.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_G.RegistroG001New: TRegistroG001;
begin
   Result := FRegistroG001;
end;

function TBloco_G.RegistroG110New: TRegistroG110;
begin
   Result := FRegistroG001.RegistroG110.New();
end;

function TBloco_G.RegistroG125New: TRegistroG125;
var
G110: TRegistroG110;
G110Count: integer;
begin
   G110Count := FRegistroG001.RegistroG110.Count -1;
   if G110Count = -1 then
      raise Exception.Create('O registro G125 deve ser filho do registro G110, e não existe nenhum G110 pai!');
   //
   G110   := FRegistroG001.RegistroG110.Items[G110Count];
   Result := G110.RegistroG125.New();
end;

function TBloco_G.RegistroG130New: TRegistroG130;
var
G125: TRegistroG125;
G110Count: integer;
G125Count: integer;
begin
   G110Count := FRegistroG001.RegistroG110.Count -1;
   G125Count := FRegistroG001.RegistroG110.Items[G110Count].RegistroG125.Count -1;
   if G125Count = -1 then
      raise Exception.Create('O registro G130 deve ser filho do registro G125, e não existe nenhum G125 pai!');
   //
   G125   := FRegistroG001.RegistroG110.Items[G110Count].RegistroG125.Items[G125Count];
   Result := G125.RegistroG130.New();
end;

function TBloco_G.RegistroG140New: TRegistroG140;
var
G130: TRegistroG130;
G110Count: integer;
G125Count: integer;
G130Count: integer;
begin
   G110Count := FRegistroG001.RegistroG110.Count -1;
   G125Count := FRegistroG001.RegistroG110.Items[G110Count].RegistroG125.Count -1;
   G130Count := FRegistroG001.RegistroG110.Items[G110Count].RegistroG125.Items[G125Count].RegistroG130.Count -1;
   if G130Count = -1 then
      raise Exception.Create('O registro G140 deve ser filho do registro G130, e não existe nenhum G130 pai!');
   //
   G130   := FRegistroG001.RegistroG110.Items[G110Count].RegistroG125.Items[G125Count].RegistroG130.Items[G130Count];
   Result := G130.RegistroG140.New(G130);
end;

procedure TBloco_G.WriteRegistroG001;
begin
  if Assigned(RegistroG001) then
  begin
     with RegistroG001 do
     begin
       Add( LFill( 'G001' ) +
            LFill( Integer(IND_MOV), 0 ) ) ;

       if IND_MOV = imComDados then
       begin
          WriteRegistroG110(FRegistroG001);
       end;
     end;

     RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
  end;
end;

procedure TBloco_G.WriteRegistroG110(RegG001: TRegistroG001);
var
  intFor: integer;
begin
  if Assigned(RegG001.RegistroG110) then
  begin
     for intFor := 0 to RegG001.RegistroG110.Count - 1 do
     begin
        with RegG001.RegistroG110.Items[intFor] do
        begin
           if FBloco_0.Registro0000.COD_VER = vlVersao102 then
           begin
              Add( LFill('G110') +
                   LFill( DT_INI) +
                   LFill( DT_FIN) +
                   LFill( MODO_CIAP ) +
                   LFill( SALDO_IN_ICMS, 0, 2 ) +
                   LFill( SALDO_FN_ICMS, 0, 2 ) +
                   LFill( SOM_PARC, 0, 2 ) +
                   LFill( VL_TRIB_EXP, 0, 2 ) +
                   LFill( VL_TOTAL, 0, 2 ) +
                   DFill( IND_PER_SAI, 8 ) +
                   LFill( ICMS_APROP, 0, 2 )+
                   LFill( SOM_ICMS_OC, 0, 2) );
           end
           else
           if FBloco_0.Registro0000.COD_VER >= vlVersao103 then
           begin
              Add( LFill('G110') +
                   LFill( DT_INI) +
                   LFill( DT_FIN) +
                   LFill( SALDO_IN_ICMS, 0, 2 ) +
                   LFill( SOM_PARC, 0, 2 ) +
                   LFill( VL_TRIB_EXP, 0, 2 ) +
                   LFill( VL_TOTAL, 0, 2 ) +
                   DFill( IND_PER_SAI, 8 ) +
                   LFill( ICMS_APROP, 0, 2 )+
                   LFill( SOM_ICMS_OC, 0, 2) );
           end;
           ///
           WriteRegistroG125( RegG001.RegistroG110.Items[intFor] );
        end;
        RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroG110Count := FRegistroG110Count + RegG001.RegistroG110.Count;
  end;
end;

procedure TBloco_G.WriteRegistroG125(RegG110: TRegistroG110);
var
  intFor: integer;
  strTIPO_MOV: String;
begin
  if Assigned( RegG110.RegistroG125 ) then
  begin
     for intFor := 0 to RegG110.RegistroG125.Count - 1 do
     begin
        with RegG110.RegistroG125.Items[intFor] do
        begin
           strTIPO_MOV := MovimentoBensToStr(TIPO_MOV);
           if FBloco_0.Registro0000.COD_VER = vlVersao102 then
           begin
              Add( LFill('G125') +
                   LFill( COD_IND_BEM ) +
                   LFill( DT_MOV ) +
                   LFill( strTIPO_MOV ) +
                   LFill( VL_IMOB_ICMS_OP, 0, 2 ) +
                   LFill( VL_IMOB_ICMS_ST, 0, 2 ) +
                   LFill( VL_IMOB_ICMS_FRT, 0, 2 ) +
                   LFill( VL_IMOB_ICMS_DIF, 0, 2 ) +
                   LFill( NUM_PARC, 3 ) +
                   LFill( VL_PARC_PASS, 0, 2 ) +
                   LFill( VL_PARC_APROP, 0, 2 ) );
           end
           else
           if FBloco_0.Registro0000.COD_VER >= vlVersao103 then
           begin
              Add( LFill('G125') +
                   LFill( COD_IND_BEM ) +
                   LFill( DT_MOV ) +
                   LFill( strTIPO_MOV ) +
                   LFill( VL_IMOB_ICMS_OP, 0, 2 ) +
                   LFill( VL_IMOB_ICMS_ST, 0, 2 ) +
                   LFill( VL_IMOB_ICMS_FRT, 0, 2 ) +
                   LFill( VL_IMOB_ICMS_DIF, 0, 2 ) +
                   LFill( NUM_PARC, 3 ) +
                   LFill( VL_PARC_PASS, 0, 2 ) );
           end;
        end;
        /// Registro FILHOS do FILHO
        if FBloco_0.Registro0000.COD_VER >= vlVersao103 then
        begin
          WriteRegistroG126( RegG110.RegistroG125.Items[intFor]);
        end;
        /// Registro FILHOS do FILHO
        WriteRegistroG130( RegG110.RegistroG125.Items[intFor]);

        RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroG125Count := FRegistroG125Count + RegG110.RegistroG125.Count;
  end;
end;

procedure TBloco_G.WriteRegistroG130(RegG125: TRegistroG125);
var
  intFor: integer;
begin
  if Assigned( RegG125.RegistroG130 ) then
  begin
     for intFor := 0 to RegG125.RegistroG130.Count - 1 do
     begin
       with RegG125.RegistroG130.Items[intFor] do
       begin
         if FBloco_0.Registro0000.COD_VER >= vlVersao113 then
         begin
           // A partir de 01/01/2020
            Add( LFill('G130') +
                 LFill( Integer(IND_EMIT), 0 ) +
                 LFill( COD_PART ) +
                 LFill( COD_MOD ) +
                 LFill( SERIE ) +
                 LFill( NUM_DOC ) +
                 LFill( CHV_NFE_CTE ) +
                 LFill( DT_DOC ) +
                 LFill( NUM_DA)
                 );
          end
          else
          begin
            Add( LFill('G130') +
                 LFill( Integer(IND_EMIT), 0 ) +
                 LFill( COD_PART ) +
                 LFill( COD_MOD ) +
                 LFill( SERIE ) +
                 LFill( NUM_DOC ) +
                 LFill( CHV_NFE_CTE ) +
                 LFill( DT_DOC ) ) ;
          end;
       end;
       WriteRegistroG140( RegG125.RegistroG130.Items[intFor]);
       RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroG130Count := FRegistroG130Count + RegG125.RegistroG130.Count;
  end;
end;

procedure TBloco_G.WriteRegistroG140(RegG130: TRegistroG130);
var
  intFor: integer;
begin
  if Assigned( RegG130.RegistroG140 ) then
  begin
    for intFor := 0 to RegG130.RegistroG140.Count - 1 do
    begin
      with RegG130.RegistroG140.Items[intFor] do
      begin
        Check(FBloco_0.Registro0001.Registro0200.LocalizaRegistro(COD_ITEM), '(G-G140) ITENS: O código do item "%s" não existe no registro 0200!', [COD_ITEM]);
        if FBloco_0.Registro0000.COD_VER >= vlVersao113 then
        begin
          // A partir de 01/01/2020
          Add( LFill('G140') +
               LFill( NUM_ITEM, 3) +
               LFill( COD_ITEM ) +
               LFill( QTDE, 0, 2 ) +
               LFill( UNID ) +
               LFill( VL_ICMS_OP_APLICADO, 0, 2 ) +
               LFill( VL_ICMS_ST_APLICADO, 0, 2 ) +
               LFill( VL_ICMS_FRT_APLICADO, 0, 2 ) +
               LFill( VL_ICMS_DIF_APLICADO, 0, 2 )
             );
        end
        else
        begin
          Add( LFill('G140') +
               LFill( NUM_ITEM, 3) +
               LFill( COD_ITEM ) ) ;
        end;
      end;
      RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
    end;
    FRegistroG140Count := FRegistroG140Count + RegG130.RegistroG140.Count;
  end;
end;

procedure TBloco_G.WriteRegistroG990 ;
begin
  if Assigned(RegistroG990) then
  begin
     with RegistroG990 do
     begin
       QTD_LIN_G := QTD_LIN_G + 1;
       ///
       Add( LFill('G990') +
            LFill(QTD_LIN_G,0) ) ;
     end;
  end;
end;

function TBloco_G.RegistroG126New: TRegistroG126;
var
G125: TRegistroG125;
G110Count: integer;
G125Count: integer;
begin
   G110Count := FRegistroG001.RegistroG110.Count -1;
   G125Count := FRegistroG001.RegistroG110.Items[G110Count].RegistroG125.Count -1;
   if G125Count = -1 then
      raise Exception.Create('O registro G126 deve ser filho do registro G125, e não existe nenhum G125 pai!');

   G125   := FRegistroG001.RegistroG110.Items[G110Count].RegistroG125.Items[G125Count];
   Result := G125.RegistroG126.New(G125);
end;

procedure TBloco_G.WriteRegistroG126(RegG125: TRegistroG125);
var
  intFor: integer;
begin
  if Assigned( RegG125.RegistroG126 ) then
  begin
     for intFor := 0 to RegG125.RegistroG126.Count - 1 do
     begin
        with RegG125.RegistroG126.Items[intFor] do
        begin
          Add( LFill('G126') +
               LFill( DT_INI) +
               LFill( DT_FIN) +
               LFill( NUM_PARC, 3 ) +
               LFill( VL_PARC_PASS, 0, 2 )+
               LFill( VL_TRIB_OC, 0, 2 ) +
               LFill( VL_TOTAL, 0, 2 ) +
               DFill( IND_PER_SAI, 8 ) +
               LFill( VL_PARC_APROP, 0, 2 )) ;
        end;
        RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroG126Count := FRegistroG126Count + RegG125.RegistroG126.Count;
  end;
end;

end.
