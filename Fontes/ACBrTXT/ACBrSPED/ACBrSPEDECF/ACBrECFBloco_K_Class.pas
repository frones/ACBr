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
|* 18/08/2015 - Ariel Guareschi - Alterado a geração do arquivo.
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_K_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_K, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_K -

  { TBloco_K }

  TBloco_K = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroK001: TRegistroK001;
    FRegistroK030: TRegistroK030List;  /// BLOCO I - Lista de RegistroJ050
    FRegistroK990: TRegistroK990;

    FRegistroK155Count: Integer;
    FRegistroK156Count: Integer;
    FRegistroK355Count: Integer;
    FRegistroK356Count: Integer;

    procedure WriteRegistroK155(RegK030: TRegistroK030);
    procedure WriteRegistroK156(RegK155: TRegistroK155);

    procedure WriteRegistroK355(RegK030: TRegistroK030);
    procedure WriteRegistroK356(RegK355: TRegistroK355);

    procedure CriaRegistros;overload;
    procedure LiberaRegistros;overload;
  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    procedure WriteRegistroK001;
    procedure WriteRegistroK030;
    procedure WriteRegistrok990;

    constructor Create;
    destructor Destroy;
    procedure LimpaRegistros;

    property RegistroK001: TRegistroK001     read FRegistroK001 write FRegistroK001;
    property RegistroK030: TRegistroK030List read FRegistroK030;
    property RegistroK990: TRegistroK990     read FRegistroK990 write FRegistroK990;

    property RegistroK155Count: Integer read FRegistroK155Count write FRegistroK155Count;
    property RegistroK156Count: Integer read FRegistroK156Count write FRegistroK156Count;
    property RegistroK355Count: Integer read FRegistroK355Count write FRegistroK355Count;
    property RegistroK356Count: Integer read FRegistroK356Count write FRegistroK356Count;
  published
  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_K }

constructor TBloco_K.Create;
begin
  FRegistroK001 := TRegistroK001.Create;
  FRegistroK030 := TRegistroK030List.Create;
  FRegistroK990 := TRegistroK990.Create;

  FRegistroK155Count := 0;
  FRegistroK156Count := 0;
  FRegistroK355Count := 0;
  FRegistroK356Count := 0;
end;

procedure TBloco_K.CriaRegistros;
begin
   inherited;

   FRegistroK001 := TRegistroK001.Create;
   FRegistroK030 := TRegistroK030List.Create;
   FRegistroK990 := TRegistroK990.Create;
   FRegistrok990.QTD_LIN := 0;

   FRegistroK155Count := 0;
   FRegistroK156Count := 0;
   FRegistroK355Count := 0;
   FRegistroK356Count := 0;
end;

destructor TBloco_K.Destroy;
begin
   FRegistroK001.Free;
   FRegistroK030.Free;
   FRegistroK990.Free;

   inherited;
end;

procedure TBloco_K.LiberaRegistros;
begin
   inherited;
   FRegistroK001.Free;
   FRegistroK030.Free;
   FRegistroK990.Free;
end;

procedure TBloco_K.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;
  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TBloco_K.WriteRegistroK001;
begin
  if Assigned(FRegistroK001) then begin
    with FRegistroK001 do begin
       Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(K-K001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       Add(LFill('K001') +
           LFill( Integer(IND_DAD), 1));
       FRegistroK990.QTD_LIN:= FRegistroK990.QTD_LIN + 1;
       WriteRegistroK030;
    end;
  end;
end;

procedure TBloco_K.WriteRegistroK030;
var
intFor: integer;
begin
  if Assigned(FRegistroK030) then begin
    for intFor := 0 to FRegistroK030.Count - 1 do begin
      with FRegistroK030.Items[intFor] do begin
        Add(LFill('K030') +
            LFill(DT_INI) +
            LFill(DT_FIN) +
            LFill(PER_APUR));
      end;
      // Registros Filhos
      WriteRegistroK155(FRegistroK030.Items[intFor]);
      WriteRegistroK355(FRegistroK030.Items[intFor]);

      FRegistroK990.QTD_LIN := FRegistroK990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_K.WriteRegistroK155(RegK030: TRegistroK030);
var
intFor: integer;
begin
  if Assigned(RegK030.RegistroK155) then begin
    for intFor := 0 to RegK030.RegistroK155.Count - 1 do begin
      with RegK030.RegistroK155.Items[intFor] do begin
        Add(LFill('K155') +
            LFill(COD_CTA) +
            LFill(COD_CCUS) +
            LFill(VL_SLD_INI, 19, 2) +
            LFill(IND_VL_SLD_INI,0) +
            LFill(VL_DEB, 19, 2) +
            LFill(VL_CRED, 19, 2) +
            LFill(VL_SLD_FIN, 19, 2) +
            LFill(IND_VL_SLD_FIN,0));
      end;
      // Registros Filhos
      WriteRegistroK156(RegK030.RegistroK155.Items[intFor]);
      FRegistroK990.QTD_LIN := FRegistroK990.QTD_LIN + 1;
    end;
    FRegistroK155Count := FRegistroK155Count + RegK030.RegistroK155.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK156(RegK155: TRegistroK155);
var
intFor: integer;
begin
  if Assigned(RegK155.RegistroK156) then begin
    for intFor := 0 to RegK155.RegistroK156.Count - 1 do begin
      with RegK155.RegistroK156.Items[intFor] do begin
        Add(LFill('K156') +
            LFill(COD_CTA_REF) +
            LFill(VL_SLD_FIN, 19, 2) +
            LFill(IND_VL_SLD_FIN,0));
      end;
      FRegistroK990.QTD_LIN := FRegistroK990.QTD_LIN + 1;
    end;
    FRegistroK156Count := FRegistroK156Count + RegK155.RegistroK156.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK355(RegK030: TRegistroK030);
var
  intFor: integer;
begin
  if Assigned(RegK030.Registrok355) then begin
    for intFor := 0 to RegK030.Registrok355.Count - 1 do begin
      with RegK030.Registrok355.Items[intFor] do begin
        Add(LFill('k355') +
            LFill(COD_CTA) +
            LFill(COD_CCUS) +
            LFill(VL_SLD_FIN, 19, 2) +
            LFill(IND_VL_SLD_FIN,0));
      end;
      // Registros Filhos
      WriteRegistroK356(RegK030.Registrok355.Items[intFor]);
      FRegistroK990.QTD_LIN := FRegistroK990.QTD_LIN + 1;
    end;
    FRegistrok355Count := FRegistrok355Count + RegK030.Registrok355.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK356(RegK355: TRegistroK355);
var
  intFor: integer;
begin
  if Assigned(RegK355.RegistroK356) then begin
    for intFor := 0 to RegK355.RegistroK356.Count - 1 do begin
      with RegK355.RegistroK356.Items[intFor] do begin
        Add(LFill('K356') +
            LFill(COD_CTA_REF) +
            LFill(VL_SLD_FIN, 19, 2) +
            LFill(IND_VL_SLD_FIN,0));
      end;
      FRegistroK990.QTD_LIN := FRegistroK990.QTD_LIN + 1;
    end;
    FRegistroK356Count := FRegistroK356Count + RegK355.RegistroK356.Count;
  end;
end;

procedure TBloco_K.WriteRegistrok990;
begin
  if Assigned(FRegistroK990) then begin
    with FRegistroK990 do begin
      QTD_LIN := QTD_LIN + 1;
      Add(LFill('K990') +
          LFill(QTD_LIN, 0));
    end;
  end;
end;

end.
