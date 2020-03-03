{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou                                 }
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

{$I ACBr.inc}

unit ACBrSEF2_BlocoF_1;

interface

Uses
  SysUtils, Classes, Contnrs,
  ACBrSEF2_BlocoF, ACBrSEF2Conversao;

type
 { TBloco_F }
  TBloco_F = class(TACBrSEFIIEDOC)
  private
    FRegistroF001 : TRegistroSEFF001;
    FRegistroF990 : TRegistroSEFF990;

    FRegistroF200Count: Integer;
    FRegistroF205Count: Integer;
    FRegistroF210Count: Integer;
    FRegistroF215Count: Integer;
    FRegistroF220Count: Integer;
    FRegistroF225Count: Integer;
    FRegistroF230Count: Integer;

    procedure WriteRegistroF200(RegF001: TRegistroSEFF001);
    procedure WriteRegistroF205(RegF001: TRegistroSEFF001);
    procedure WriteRegistroF210(RegF001: TRegistroSEFF001);
    procedure WriteRegistroF215(RegF210: TRegistroSEFF210);
    procedure WriteRegistroF220(RegF215: TRegistroSEFF215);
    procedure WriteRegistroF225(RegF220: TRegistroSEFF220);
    procedure WriteRegistroF230(RegF215: TRegistroSEFF215);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LimpaRegistros;

    function RegistroF001New : TRegistroSEFF001;
    function RegistroF200New : TRegistroSEFF200;
    function RegistroF205New : TRegistroSEFF205;
    function RegistroF210New : TRegistroSEFF210;
    function RegistroF215New : TRegistroSEFF215;
    function RegistroF220New : TRegistroSEFF220;
    function RegistroF225New : TRegistroSEFF225;
    function RegistroF230New : TRegistroSEFF230;

    procedure WriteRegistroF001;
    procedure WriteRegistroF990;

    property RegistroF001: TRegistroSEFF001 read FRegistroF001 write FRegistroF001;
    property RegistroF990: TRegistroSEFF990 read FRegistroF990 write FRegistroF990;
    
	  property RegistroF200Count: Integer read FRegistroF200Count write FRegistroF200Count;
    property RegistroF205Count: Integer read FRegistroF205Count write FRegistroF205Count;
    property RegistroF210Count: Integer read FRegistroF210Count write FRegistroF210Count;
    property RegistroF215Count: Integer read FRegistroF215Count write FRegistroF215Count;
    property RegistroF220Count: Integer read FRegistroF220Count write FRegistroF220Count;
    property RegistroF225Count: Integer read FRegistroF225Count write FRegistroF225Count;
    property RegistroF230Count: Integer read FRegistroF230Count write FRegistroF230Count;
  end;

implementation

function IntToStrNull(AInteger : Integer) : string;
begin
  if AInteger = 0 then
    Result := ''
  else
    Result := IntToStr(AInteger);
end;

{ TBloco_F }

constructor TBloco_F.Create;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_F.Destroy;
begin
   LiberaRegistros;
   inherited;
end;

procedure TBloco_F.WriteRegistroF001;
var
  Astr: String;
begin
  if Assigned(FRegistroF001) then
  begin
    with FRegistroF001 do
    begin
      Astr := LFill( 'F001' )+
              LFill(Integer(IND_DAD), 1);
      Add(Astr);

      WriteRegistroF200(FRegistroF001);
      WriteRegistroF205(FRegistroF001);
      WriteRegistroF210(FRegistroF001);
    end;

    RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
  end;
end;

procedure TBloco_F.WriteRegistroF200(RegF001: TRegistroSEFF001);
var
 intFor : Integer;
 RegF200 : TRegistroSEFF200;
begin
  for intFor := 0 to RegF001.RegistroF200.Count - 1 do
  begin
    RegF200 := TRegistroSEFF200(RegF001.RegistroF200.Items[intFor]);
    with RegF200 do
    begin
      Add( LFill('F200')                    +
           LFill(IntToStr(IND_LMC))         +
           LFill(DT_INI)                    +
           LFill(DT_FIN));
    end;
    RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
  end;

  FRegistroF200Count := FRegistroF200Count + RegF001.RegistroF200.Count;
end;

procedure TBloco_F.WriteRegistroF205(RegF001: TRegistroSEFF001);
var
 intFor : Integer;
 RegF205 : TRegistroSEFF205;
begin
   for intFor := 0 to RegF001.RegistroF205.Count - 1 do
   begin
     RegF205 := TRegistroSEFF205(RegF001.RegistroF205.Items[intFor]);
     with RegF205 do
     begin
        Add( LFill('F205')   +
             LFill(BOMBA)    +
             LFill(EQP_FAB)  +
             LFill(EQP_CNPJ) +
             LFill(EQP_MOD)  +
             LFill(IND_BOMB,1));
     end;
    RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
   end;

   FRegistroF205Count := FRegistroF205Count + RegF001.RegistroF200.Count;
end;

procedure TBloco_F.WriteRegistroF210(RegF001: TRegistroSEFF001);
var
 intFor : Integer;
 RegF210 : TRegistroSEFF210;
begin
   for intFor := 0 to RegF001.RegistroF210.Count - 1 do
   begin
     RegF210 := TRegistroSEFF210(RegF001.RegistroF210.Items[intFor]);
     with RegF210 do
     begin
        Add( LFill('F210')   +
             LFill(COD_ITEM) +
             LFill(COD_INF_OBS));
     end;

     WriteRegistroF215(RegF210);
     FRegistroF990.QTD_LIN_F := FRegistroF990.QTD_LIN_F + 1;
   end;

   FRegistroF210Count := FRegistroF210Count + RegF001.RegistroF210.Count;
end;

procedure TBloco_F.WriteRegistroF215(RegF210: TRegistroSEFF210);
var
 intFor : Integer;
 RegF215 : TRegistroSEFF215;
begin
  for intFor := 0 to RegF210.RegistroF215.Count - 1 do
  begin
    RegF215 := TRegistroSEFF215(RegF210.RegistroF215.Items[intFor]);
    with RegF215 do
    begin
     Add( LFill('F215')               +
          LFill(IND_TOT, 1)           +
          LFill(DT_MOV)               +
          LFill(ESTQ_INI, 0, 3, True, '0', '0.000') +
          LFill(ENTRADAS, 0, 3, True, '0', '0.000') +
          LFill(SAIDAS, 0, 3, True, '0', '0.000')   +
          LFill(ESTQ_FIN, 0, 3, True, '0', '0.000') +
          LFill(ESTQ_FECH, 0, 3, True, '0', '0.000')+
          LFill(PERDAS, 0, 3, True, '0', '0.000')   +
          LFill(SOBRAS, 0, 3, True, '0', '0.000')   +
          LFill(VL_UNIT, 0, 3, True, '0', '0.000')  +
          LFill(VL_SAID, 0, 3, True));
    end;

    WriteRegistroF220(RegF215);
    WriteRegistroF230(RegF215);
    FRegistroF990.QTD_LIN_F := FRegistroF990.QTD_LIN_F + 1;
  end;

  FRegistroF215Count := FRegistroF215Count + RegF210.RegistroF215.Count;
end;

procedure TBloco_F.WriteRegistroF220(RegF215: TRegistroSEFF215);
var
 intFor : Integer;
 RegF220 : TRegistroSEFF220;
begin
  for intFor := 0 to RegF215.RegistroF220.Count - 1 do
  begin
    RegF220 := TRegistroSEFF220(RegF215.RegistroF220.Items[intFor]);
    with RegF220 do
    begin
     Add( LFill('F220')                             +
          LFill(TANQUE)                             +
          LFill(VOL_INI, 0, 3, True, '0', '0.000')  +
          LFill(VOL_ENTR, 0, 3, True, '0', '0.000') +
          LFill(VOL_SAID, 0, 3, True, '0', '0.000') +
          LFill(VOL_FECH, 0, 3, True, '0', '0.000') +
          LFill(VOL_PRD, 0, 3, True, '0', '0.000')  +
          LFill(VOL_SBR, 0, 3, True, '0', '0.000'));
    end;

    WriteRegistroF225(RegF220);
    FRegistroF990.QTD_LIN_F := FRegistroF990.QTD_LIN_F + 1;
  end;
  FRegistroF220Count := FRegistroF220Count + RegF215.RegistroF220.Count;
end;

procedure TBloco_F.WriteRegistroF225(RegF220: TRegistroSEFF220);
var
 intFor : Integer;
 RegF225 : TRegistroSEFF225;
begin
   for intFor := 0 to RegF220.RegistroF225.Count - 1 do
   begin
     RegF225 := TRegistroSEFF225(RegF220.RegistroF225.Items[intFor]);
     with RegF225 do
     begin
        Add( LFill('F225')                        +
             LFill(IND_OPER,1)                    +
             LFill(IND_EMIT)                      +
             LFill(CNPJ)                          +
             LFill(UF)                            +
             LFill(IE)                            +
             LFill(ModDocumentoToStr(COD_MOD))    +
             LFill(SER)                           +
             LFill(NUM_DOC,0)                     +
             LFill(DT_DOC)                        +
             LFill(VL_DOC, 0, 2, True)            +
             LFill(VL_ICMS, 0, 2, True)           +
             LFill(VL_ICMS_ST, 0, 2, True)        +
             LFill(VOL, 0, 3, True, '0', '0.000') +
             LFill(COD_INF_OBS));
     end;
    RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
   end;

   FRegistroF225Count := FRegistroF225Count + RegF220.RegistroF225.Count;
end;

procedure TBloco_F.WriteRegistroF230(RegF215: TRegistroSEFF215);
var
 intFor : Integer;
 RegF230: TRegistroSEFF230;
begin
   for intFor := 0 to RegF215.RegistroF230.Count - 1 do
   begin
     RegF230 := TRegistroSEFF230(RegF215.RegistroF230.Items[intFor]);
     with RegF230 do
     begin
        Add( LFill('F230')               +
             LFill(BOMBA)                +
             LFill(BICO)                 +
             LFill(ENC_FIN, 0, 3, True)  +
             LFill(ENC_INI, 0, 3, True)  +
             LFill(VOL_AFER, 0, 2, True));
     end;
    RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
   end;

   FRegistroF230Count := FRegistroF230Count + RegF215.RegistroF230.Count;
end;

procedure TBloco_F.WriteRegistroF990;
var
  strLinha : String;
begin
   strLinha := '';

   if Assigned(RegistroF990) then
   begin
      with RegistroF990 do
      begin
        QTD_LIN_F := QTD_LIN_F + 1;

        strLinha := LFill('F990') +
                    LFill(QTD_LIN_F,0);
        Add(strLinha);
      end;
   end;
end;

procedure TBloco_F.CriaRegistros;
begin
  FRegistroF001 := TRegistroSEFF001.Create;
  FRegistroF990 := TRegistroSEFF990.Create;

  FRegistroF200Count := 0;
  FRegistroF205Count := 0;
  FRegistroF210Count := 0;
  FRegistroF215Count := 0;

  FRegistroF990.QTD_LIN_F := 0;
end;

procedure TBloco_F.LiberaRegistros;
begin
  FRegistroF001.Free;
  FRegistroF990.Free;
end;

procedure TBloco_F.LimpaRegistros;
begin
   /// Limpa os Registros
   LiberaRegistros;
   Conteudo.Clear;

   /// Recriar os Registros Limpos
   CriaRegistros;
end;

function TBloco_F.RegistroF001New: TRegistroSEFF001;
begin
   Result := FRegistroF001;
end;

function TBloco_F.RegistroF200New : TRegistroSEFF200;
begin
  Result :=  FRegistroF001.RegistroF200.New();
end;

function TBloco_F.RegistroF205New: TRegistroSEFF205;
begin
  Result := FRegistroF001.RegistroF205.New();
end;

function TBloco_F.RegistroF210New: TRegistroSEFF210;
begin
  Result := FRegistroF001.RegistroF210.New();
end;

function TBloco_F.RegistroF215New: TRegistroSEFF215;
var
  F210: TRegistroSEFF210;
begin
   with FRegistroF001.RegistroF210 do
     F210 := TRegistroSEFF210(Items[ AchaUltimoPai('F210', 'F215') ]);

   Result := F210.RegistroF215.New();
end;

function TBloco_F.RegistroF220New: TRegistroSEFF220;
var
  F215: TRegistroSEFF215;
begin
   with FRegistroF001.RegistroF215 do
     F215 := TRegistroSEFF215(Items[ AchaUltimoPai('F215', 'F220') ]);

   Result := F215.RegistroF220.New();
end;

function TBloco_F.RegistroF225New: TRegistroSEFF225;
var
  F215: TRegistroSEFF215;
  F220: TRegistroSEFF220;
begin
   with FRegistroF001.RegistroF215 do
     F215 := TRegistroSEFF215(Items[ AchaUltimoPai('F215', 'F225') ]);

   with F215.RegistroF220 do
     F220 := TRegistroSEFF220(Items[ AchaUltimoPai('F220', 'F225') ]);

   Result := F220.RegistroF225.New();
end;

function TBloco_F.RegistroF230New: TRegistroSEFF230;
var
  F215: TRegistroSEFF215;
begin
   with FRegistroF001.RegistroF215 do
     F215 := TRegistroSEFF215(Items[ AchaUltimoPai('F215', 'F230') ]);

   Result := F215.RegistroF230.New();
end;

end.
