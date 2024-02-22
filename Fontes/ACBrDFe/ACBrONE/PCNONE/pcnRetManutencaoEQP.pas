{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit pcnRetManutencaoEQP;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao,
  pcnLeitor;

type
  { TRetManutencaoEQP }

  TRetManutencaoEQP = class(TObject)
  private
    FLeitor: TLeitor;

    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FdhResp: TDateTime;
    FNSUMovto: String;
    FXML: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;

    property Leitor: TLeitor         read FLeitor   write FLeitor;
    property versao: String          read Fversao   write Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb    write FtpAmb;
    property verAplic: String        read FverAplic write FverAplic;
    property cStat: Integer          read FcStat    write FcStat;
    property xMotivo: String         read FxMotivo  write FxMotivo;
    property dhResp: TDateTime       read FdhResp   write FdhResp;
    property NSUMovto: String        read FNSUMovto write FNSUMovto;
    property XML: AnsiString         read FXML      write FXML;
  end;

implementation

uses
  ACBrONEConversao;

{ TRetManutencaoEQP }

constructor TRetManutencaoEQP.Create;
begin
  inherited;

  FLeitor := TLeitor.Create;
end;

destructor TRetManutencaoEQP.Destroy;
begin
  FLeitor.Free;

  inherited;
end;

function TRetManutencaoEQP.LerXml: Boolean;
var
  ok: Boolean;
begin
  Result := False;

  try
    if (Leitor.rExtrai(1, 'retOneManEQP') <> '') then
    begin
      Fversao   := Leitor.rAtributo('versao');
      FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      FverAplic := Leitor.rCampo(tcStr, 'verAplic');
      FcStat    := Leitor.rCampo(tcInt, 'cStat');
      FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
      FdhResp   := Leitor.rCampo(tcDatHor, 'dhResp');
      FNSUMovto := Leitor.rCampo(tcStr, 'NSUMovto');

      Result := True;
    end;

    if (Leitor.rExtrai(1, 'oneManEQP') <> '') then
    begin
      {
      i := 0;
      while Leitor.rExtrai(2, 'infEvento', '', i + 1) <> '' do
       begin
         FretEvento.New;

         FretEvento.Items[i].FRetInfEvento.XML := Leitor.Grupo;

         FretEvento.Items[i].FRetInfEvento.Id         := Leitor.rAtributo('Id');
         FretEvento.Items[i].FRetInfEvento.tpAmb      := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
         FretEvento.Items[i].FRetInfEvento.verAplic   := Leitor.rCampo(tcStr, 'verAplic');
         FretEvento.Items[i].FRetInfEvento.cOrgao     := Leitor.rCampo(tcInt, 'cOrgao');
         FretEvento.Items[i].FRetInfEvento.cStat      := Leitor.rCampo(tcInt, 'cStat');
         FretEvento.Items[i].FRetInfEvento.xMotivo    := Leitor.rCampo(tcStr, 'xMotivo');

         FretEvento.Items[i].FRetInfEvento.chONE      := Leitor.rCampo(tcStr, 'chONE');
         // Alterado a função de conversao
         FretEvento.Items[i].FRetInfEvento.tpEvento   := StrToTpEventoONE(ok, Leitor.rCampo(tcStr, 'tpEvento'));
         FretEvento.Items[i].FRetInfEvento.xEvento    := Leitor.rCampo(tcStr, 'xEvento');
         FretEvento.Items[i].FRetInfEvento.nSeqEvento := Leitor.rCampo(tcInt, 'nSeqEvento');
         FretEvento.Items[i].FRetInfEvento.dhRegEvento := Leitor.rCampo(tcDatHor, 'dhRegEvento');
         FretEvento.Items[i].FRetInfEvento.nProt       := Leitor.rCampo(tcStr, 'nProt');

         FretEvento.Items[i].FRetInfEvento.CNPJDest   := Leitor.rCampo(tcStr, 'CNPJDest');

         if FretEvento.Items[i].FRetInfEvento.CNPJDest = '' then
           FretEvento.Items[i].FRetInfEvento.CNPJDest  := Leitor.rCampo(tcStr, 'CPFDest');

         FretEvento.Items[i].FRetInfEvento.emailDest   := Leitor.rCampo(tcStr, 'emailDest');
         FretEvento.Items[i].FRetInfEvento.cOrgaoAutor := Leitor.rCampo(tcInt, 'cOrgaoAutor');

         j := 0;
         while  Leitor.rExtrai(3, 'chONEPend', '', j + 1) <> '' do
          begin
            FretEvento.Items[i].FRetInfEvento.chONEPend.New;

            FretEvento.Items[i].FRetInfEvento.chONEPend[j].ChavePend := Leitor.rCampo(tcStr, 'chONEPend');

            inc(j);
          end;

         inc(i);
       end;
      }
      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.
