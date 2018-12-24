{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014   Juliomar Marchetti                   }
{					                    2015   Isaque Pinheiro	    	             }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 11/09/2015 - Ariel Guareschi - Identar no padrao utilizado pela ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_N_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_N, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_N -

  { TBloco_N }

  TBloco_N = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroN001: TRegistroN001;
    FRegistroN990: TRegistroN990;

    FRegistroN030Count: Integer;
    FRegistroN500Count: Integer;
    FRegistroN600Count: Integer;
    FRegistroN610Count: Integer;
    FRegistroN615Count: Integer;
    FRegistroN620Count: Integer;
    FRegistroN630Count: Integer;
    FRegistroN650Count: Integer;
    FRegistroN660Count: Integer;
    FRegistroN670Count: Integer;

    procedure WriteRegistroN030(RegN001: TRegistroN001);
    procedure WriteRegistroN500(RegN030: TRegistroN030);
    procedure WriteRegistroN600(RegN030: TRegistroN030);
    procedure WriteRegistroN610(RegN030: TRegistroN030);
    procedure WriteRegistroN615(RegN030: TRegistroN030);
    procedure WriteRegistroN620(RegN030: TRegistroN030);
    procedure WriteRegistroN630(RegN030: TRegistroN030);
    procedure WriteRegistroN650(RegN030: TRegistroN030);
    procedure WriteRegistroN660(RegN030: TRegistroN030);
    procedure WriteRegistroN670(RegN030: TRegistroN030);

    procedure CriaRegistros;overload;
    procedure LiberaRegistros;overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LimpaRegistros; override;

    function RegistroN001New :TRegistroN001;
    function RegistroN030New :TRegistroN030;
    function RegistroN500New :TRegistroN500;
    function RegistroN600New :TRegistroN600;
    function RegistroN610New :TRegistroN610;
    function RegistroN615New :TRegistroN615;
    function RegistroN620New :TRegistroN620;
    function RegistroN630New :TRegistroN630;
    function RegistroN650New :TRegistroN650;
    function RegistroN660New :TRegistroN660;
    function RegistroN670New :TRegistroN670;

    procedure WriteRegistroN001;
    procedure WriteRegistroN990;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroN001: TRegistroN001 read FRegistroN001 write FRegistroN001;
    property RegistroN990: TRegistroN990 read FRegistroN990 write FRegistroN990;

    property RegistroN030Count: Integer read FRegistroN030Count write FRegistroN030Count;
    property RegistroN500Count: Integer read FRegistroN500Count write FRegistroN500Count;
    property RegistroN600Count: Integer read FRegistroN600Count write FRegistroN600Count;
    property RegistroN610Count: Integer read FRegistroN610Count write FRegistroN610Count;
    property RegistroN615Count: Integer read FRegistroN615Count write FRegistroN615Count;
    property RegistroN620Count: Integer read FRegistroN620Count write FRegistroN620Count;
    property RegistroN630Count: Integer read FRegistroN630Count write FRegistroN630Count;
    property RegistroN650Count: Integer read FRegistroN650Count write FRegistroN650Count;
    property RegistroN660Count: Integer read FRegistroN660Count write FRegistroN660Count;
    property RegistroN670Count: Integer read FRegistroN670Count write FRegistroN670Count;
  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_N }

constructor TBloco_N.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TBloco_N.CriaRegistros;
begin
  inherited;
  FRegistroN001 := TRegistroN001.Create;
  FRegistroN990 := TRegistroN990.Create;

  FRegistroN030Count := 0;
  FRegistroN500Count := 0;
  FRegistroN600Count := 0;
  FRegistroN610Count := 0;
  FRegistroN615Count := 0;
  FRegistroN620Count := 0;
  FRegistroN630Count := 0;
  FRegistroN650Count := 0;
  FRegistroN660Count := 0;
  FRegistroN670Count := 0;

  FRegistroN990.QTD_LIN := 0;
end;

destructor TBloco_N.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_N.LiberaRegistros;
begin
  inherited;
  FRegistroN001.Free;
  FRegistroN990.Free;
end;

procedure TBloco_N.LimpaRegistros;
begin
  LiberaRegistros;
  Conteudo.Clear;

  CriaRegistros;
end;

function TBloco_N.RegistroN001New: TRegistroN001;
begin
  Result := FRegistroN001;
end;

function TBloco_N.RegistroN030New: TRegistroN030;
begin
  Result := FRegistroN001.RegistroN030.New;
end;

function TBloco_N.RegistroN500New: TRegistroN500;
var
  UN030: TRegistroN030;
  UN030Count: Integer;
begin
  UN030Count := FRegistroN001.RegistroN030.Count -1;
  if UN030Count = -1 then
    raise Exception.Create('O registro N500 deve ser filho do registro N030, e não existe nenhum N030 pai!');

  UN030  := FRegistroN001.RegistroN030.Items[UN030Count];
  Result := UN030.RegistroN500.New;
end;

function TBloco_N.RegistroN600New: TRegistroN600;
var
  UN030: TRegistroN030;
  UN030Count: Integer;
begin
  UN030Count := FRegistroN001.RegistroN030.Count -1;
  if UN030Count = -1 then
    raise Exception.Create('O registro N600 deve ser filho do registro N030, e não existe nenhum N030 pai!');

  UN030  := FRegistroN001.RegistroN030.Items[UN030Count];
  Result := UN030.RegistroN600.New;
end;

function TBloco_N.RegistroN610New: TRegistroN610;
var
  UN030: TRegistroN030;
  UN030Count: Integer;
begin
  UN030Count := FRegistroN001.RegistroN030.Count -1;
  if UN030Count = -1 then
    raise Exception.Create('O registro N610 deve ser filho do registro N030, e não existe nenhum N030 pai!');

  UN030  := FRegistroN001.RegistroN030.Items[UN030Count];
  Result := UN030.RegistroN610.New;
end;

function TBloco_N.RegistroN615New: TRegistroN615;
var
  UN030: TRegistroN030;
  UN030Count: Integer;
begin
  UN030Count := FRegistroN001.RegistroN030.Count -1;
  if UN030Count = -1 then
    raise Exception.Create('O registro N615 deve ser filho do registro N030, e não existe nenhum N030 pai!');

  UN030  := FRegistroN001.RegistroN030.Items[UN030Count];
  Result := UN030.RegistroN615.New;
end;

function TBloco_N.RegistroN620New: TRegistroN620;
var
  UN030: TRegistroN030;
  UN030Count: Integer;
begin
  UN030Count := FRegistroN001.RegistroN030.Count -1;
  if UN030Count = -1 then
    raise Exception.Create('O registro N620 deve ser filho do registro N030, e não existe nenhum N030 pai!');

  UN030  := FRegistroN001.RegistroN030.Items[UN030Count];
  Result := UN030.RegistroN620.New;
end;

function TBloco_N.RegistroN630New: TRegistroN630;
var
  UN030: TRegistroN030;
  UN030Count: Integer;
begin
  UN030Count := FRegistroN001.RegistroN030.Count -1;
  if UN030Count = -1 then
    raise Exception.Create('O registro N630 deve ser filho do registro N030, e não existe nenhum N030 pai!');

  UN030  := FRegistroN001.RegistroN030.Items[UN030Count];
  Result := UN030.RegistroN630.New;
end;

function TBloco_N.RegistroN650New: TRegistroN650;
var
  UN030: TRegistroN030;
  UN030Count: Integer;
begin
  UN030Count := FRegistroN001.RegistroN030.Count -1;
  if UN030Count = -1 then
    raise Exception.Create('O registro N650 deve ser filho do registro N030, e não existe nenhum N030 pai!');

  UN030  := FRegistroN001.RegistroN030.Items[UN030Count];
  Result := UN030.RegistroN650.New;
end;

function TBloco_N.RegistroN660New: TRegistroN660;
var
  UN030: TRegistroN030;
  UN030Count: Integer;
begin
  UN030Count := FRegistroN001.RegistroN030.Count -1;
  if UN030Count = -1 then
    raise Exception.Create('O registro N660 deve ser filho do registro N030, e não existe nenhum N030 pai!');

  UN030  := FRegistroN001.RegistroN030.Items[UN030Count];
  Result := UN030.RegistroN660.New;
end;

function TBloco_N.RegistroN670New: TRegistroN670;
var
  UN030: TRegistroN030;
  UN030Count: Integer;
begin
  UN030Count := FRegistroN001.RegistroN030.Count -1;
  if UN030Count = -1 then
    raise Exception.Create('O registro N670 deve ser filho do registro N030, e não existe nenhum N030 pai!');

  UN030  := FRegistroN001.RegistroN030.Items[UN030Count];
  Result := UN030.RegistroN670.New;
end;

procedure TBloco_N.WriteRegistroN001;
begin
  if Assigned(FRegistroN001) then
  begin
    with FRegistroN001 do
    begin
      Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(N-N001) Na abertura do bloco, deve ser informado o número 0 ou 1!');

      Add( LFill('N001') +
           LFill( Integer(IND_DAD), 1) );

      if (IND_DAD = idComDados) then
      begin
        WriteRegistroN030(FRegistroN001);
      end;

      FRegistroN990.QTD_LIN:= FRegistroN990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_N.WriteRegistroN030(RegN001: TRegistroN001);
var
  intFor: integer;
begin
  if Assigned(RegN001.RegistroN030) then
  begin
    for intFor := 0 to RegN001.RegistroN030.Count - 1 do
    begin
      with RegN001.RegistroN030.Items[intFor] do
      begin
        Add( LFill('N030') +
             LFill(DT_INI) +
             LFill(DT_FIN) +
             LFill(PER_APUR));
      end;

      // Registros Filhos
      WriteRegistroN500(RegN001.RegistroN030.Items[intFor]);
      WriteRegistroN600(RegN001.RegistroN030.Items[intFor]);
      WriteRegistroN610(RegN001.RegistroN030.Items[intFor]);
      WriteRegistroN615(RegN001.RegistroN030.Items[intFor]);
      WriteRegistroN620(RegN001.RegistroN030.Items[intFor]);
      WriteRegistroN630(RegN001.RegistroN030.Items[intFor]);
      WriteRegistroN650(RegN001.RegistroN030.Items[intFor]);
      WriteRegistroN660(RegN001.RegistroN030.Items[intFor]);
      WriteRegistroN670(RegN001.RegistroN030.Items[intFor]);

      FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
    end;

    FRegistroN030Count := FRegistroN030Count + RegN001.RegistroN030.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN500(RegN030: TRegistroN030);
var
  intFor: integer;
begin
  if Assigned(RegN030.RegistroN500) then
  begin
    for intFor := 0 to RegN030.RegistroN500.Count - 1 do
    begin
      with RegN030.RegistroN500.Items[intFor] do
      begin

        Add(LFill('N500')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
    end;

    FRegistroN500Count := FRegistroN500Count + RegN030.RegistroN500.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN600(RegN030: TRegistroN030);
var
  intFor: integer;
begin
  if Assigned(RegN030.RegistroN600) then
  begin
    for intFor := 0 to RegN030.RegistroN600.Count - 1 do
    begin
      with RegN030.RegistroN600.Items[intFor] do
      begin

        Add(LFill('N600')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
    end;

    FRegistroN600Count := FRegistroN600Count + RegN030.RegistroN600.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN610(RegN030: TRegistroN030);
var
  intFor: integer;
begin
  if Assigned(RegN030.RegistroN610) then
  begin
    for intFor := 0 to RegN030.RegistroN610.Count - 1 do
    begin
      with RegN030.RegistroN610.Items[intFor] do
      begin

        Add(LFill('N610')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
    end;

    FRegistroN610Count := FRegistroN610Count + RegN030.RegistroN610.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN615(RegN030: TRegistroN030);
var
  intFor: integer;
begin
  if Assigned(RegN030.RegistroN615) then
  begin
    for intFor := 0 to RegN030.RegistroN615.Count - 1 do
    begin
      with RegN030.RegistroN615.Items[intFor] do
      begin

        Add(LFill('N615')                     +
            VLFill(BASE_CALC, 19, 2)           +
            VLFill(PER_INCEN_FINOR, 5, 2)      +
            VLFill(VL_LIQ_INCEN_FINOR, 19, 2)  +
            VLFill(PER_INCEN_FINAM, 5, 2)      +
            VLFill(VL_LIQ_INCEN_FINAM, 19, 2)  +
            VLFill(VL_SUBTOTAL, 19, 2)         +
            VLFill(PER_INCEN_FUNRES, 5, 2)     +
            VLFill(VL_LIQ_INCEN_FUNRES, 19, 2) +
            VLFill(VL_TOTAL, 19, 2));
      end;

      FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
    end;

    FRegistroN615Count := FRegistroN615Count + RegN030.RegistroN615.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN620(RegN030: TRegistroN030);
var
  intFor: integer;
begin
  if Assigned(RegN030.RegistroN620) then
  begin
    for intFor := 0 to RegN030.RegistroN620.Count - 1 do
    begin
      with RegN030.RegistroN620.Items[intFor] do
      begin

        Add(LFill('N620')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
    end;

    FRegistroN620Count := FRegistroN620Count + RegN030.RegistroN620.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN630(RegN030: TRegistroN030);
var
  intFor: integer;
begin
  if Assigned(RegN030.RegistroN630) then
  begin
    for intFor := 0 to RegN030.RegistroN630.Count - 1 do
    begin
      with RegN030.RegistroN630.Items[intFor] do
      begin

        Add(LFill('N630')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
    end;

    FRegistroN630Count := FRegistroN630Count + RegN030.RegistroN630.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN650(RegN030: TRegistroN030);
var
  intFor: integer;
begin
  if Assigned(RegN030.RegistroN650) then
  begin
    for intFor := 0 to RegN030.RegistroN650.Count - 1 do
    begin
      with RegN030.RegistroN650.Items[intFor] do
      begin

        Add(LFill('N650')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
    end;

    FRegistroN650Count := FRegistroN650Count + RegN030.RegistroN650.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN660(RegN030: TRegistroN030);
var
  intFor: integer;
begin
  if Assigned(RegN030.RegistroN660) then
  begin
    for intFor := 0 to RegN030.RegistroN660.Count - 1 do
    begin
      with RegN030.RegistroN660.Items[intFor] do
      begin

        Add(LFill('N660')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
    end;

    FRegistroN660Count := FRegistroN660Count + RegN030.RegistroN660.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN670(RegN030: TRegistroN030);
var
  intFor: integer;
begin
  if Assigned(RegN030.RegistroN670) then
  begin
    for intFor := 0 to RegN030.RegistroN670.Count - 1 do
    begin
      with RegN030.RegistroN670.Items[intFor] do
      begin

        Add(LFill('N670')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
    end;

    FRegistroN670Count := FRegistroN670Count + RegN030.RegistroN670.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN990;
begin
  if Assigned(FRegistroN990) then
  begin
    with FRegistroN990 do
    begin
      QTD_LIN := QTD_LIN + 1;

      Add( LFill('N990') +
           LFill(QTD_LIN, 0) );
    end;
  end;
end;

end.
