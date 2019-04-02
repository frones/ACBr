{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}
{$I ACBr.inc}

unit ACBrGNREGuiasRetorno;

interface

uses
  Classes, Sysutils, Dialogs, Forms,
  ACBrGNREGuiaClass,
  pgnreGNRERetorno, pgnreConversao, pcnConversao, pcnAuxiliar, pcnLeitor;

type

  GuiaRetorno = class(TCollectionItem)
  private
    FGNRE: TGNRERetorno;
    FConfirmada : Boolean;
    FMsg : AnsiString ;
    FAlertas: AnsiString;
    FNomeArq: String;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF;
    property GNRE: TGNRERetorno  read FGNRE write FGNRE;
    property Confirmada: Boolean  read FConfirmada write FConfirmada;
    property Msg: AnsiString  read FMsg write FMsg;
    property Alertas: AnsiString read FAlertas write FAlertas;
    property NomeArq: String read FNomeArq write FNomeArq;
  end;

  TGuiasRetorno = class(TOwnedCollection)
  private
    FACBrGNRE : TComponent;
    function GetItem(Index: Integer): GuiaRetorno;
    procedure SetItem(Index: Integer; const Value: GuiaRetorno);
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    procedure Imprimir;
    procedure ImprimirPDF;
    function Add: GuiaRetorno;
    function Insert(Index: Integer): GuiaRetorno;
    property Items[Index: Integer]: GuiaRetorno read GetItem  write SetItem;
    function GetNamePath: string; override ;
    function LoadFromFile(CaminhoArquivo: String): boolean;
    function LoadFromString(Arquivo: String): boolean;
    property ACBrGNRE : TComponent read FACBrGNRE ;
  end;

implementation

uses
 ACBrGNRE2, ACBrUtil, ACBrDFeUtil, pcnGerador;

{ GuiaRetorno }

constructor GuiaRetorno.Create(Collection2: TCollection);
begin
 inherited Create(Collection2);
 FGNRE     := TGNRERetorno.Create;
 FNomeArq  := '';
end;

destructor GuiaRetorno.Destroy;
begin
  FGNRE.Free;
  inherited Destroy;
end;

procedure GuiaRetorno.Imprimir;
begin
 if not Assigned( TACBrGNRE( TGuiasRetorno( Collection ).ACBrGNRE ).GNREGuia ) then
   raise Exception.Create('Componente GNREGuia não associado.')
 else
   TACBrGNRE( TGuiasRetorno( Collection ).ACBrGNRE ).GNREGuia.ImprimirGuia(GNRE);
end;

procedure GuiaRetorno.ImprimirPDF;
begin
 if not Assigned( TACBrGNRE( TGuiasRetorno( Collection ).ACBrGNRE ).GNREGuia ) then
   raise Exception.Create('Componente DANFSE não associado.')
 else
   TACBrGNRE( TGuiasRetorno( Collection ).ACBrGNRE ).GNREGuia.ImprimirGuiaPDF(GNRE);
end;

{ TGuiaRetorno }

function TGuiasRetorno.Add: GuiaRetorno;
begin
  Result := GuiaRetorno(inherited Add);
end;

constructor TGuiasRetorno.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
 if not (AOwner is TACBrGNRE ) then
   raise Exception.Create( 'AOwner deve ser do tipo TGNRE.') ;

 inherited;
 FACBrGNRE := TACBrGNRE( AOwner ) ;
end;

function TGuiasRetorno.GetItem(Index: Integer): GuiaRetorno;
begin
  Result := GuiaRetorno(inherited Items[Index]);
end;

function TGuiasRetorno.GetNamePath: string;
begin
  Result := 'Guia';
end;

procedure TGuiasRetorno.Imprimir;
begin
 if not Assigned( TACBrGNRE( FACBrGNRE ).GNREGuia ) then
   raise Exception.Create('Componente GNREGuia não associado.')
 else
   TACBrGNRE( FACBrGNRE ).GNREGuia.ImprimirGuia(nil);
end;

procedure TGuiasRetorno.ImprimirPDF;
begin
 if not Assigned( TACBrGNRE( FACBrGNRE ).GNREGuia ) then
   raise Exception.Create('Componente GNREGuia não associado.')
 else
   TACBrGNRE( FACBrGNRE ).GNREGuia.ImprimirGuiaPDF(nil);
end;

function TGuiasRetorno.Insert(Index: Integer): GuiaRetorno;
begin
  Result := GuiaRetorno(inherited Insert(Index));
end;

function TGuiasRetorno.LoadFromFile(CaminhoArquivo: string): boolean;
var
  GNRERetorno: TGNRERetorno;
  ArquivoRetorno: TStringList;
  i: Integer;
begin
  try
    ArquivoRetorno := TStringList.Create;
    ArquivoRetorno.LoadFromFile(CaminhoArquivo);
    Result := True;

    GNRERetorno := TACBrGNRE(ACBrGNRE).GuiasRetorno.Add.GNRE;    
    for i := 0 to ArquivoRetorno.Count - 1 do
    begin
      if SameText(Copy(ArquivoRetorno.Strings[i], 1, 1), '0') then
      begin
        GNRERetorno.InfoCabec.TipoIdentificadoSolicitante := StrToInt(Copy(ArquivoRetorno.Strings[i], 2, 1));
        GNRERetorno.InfoCabec.IdentificadorSolicitante := Trim(Copy(ArquivoRetorno.Strings[i], 3, 14));
        GNRERetorno.InfoCabec.NumeroProtocoloLote := Trim(Copy(ArquivoRetorno.Strings[i], 17, 10));
        GNRERetorno.InfoCabec.Ambiente := StrToInt(Copy(ArquivoRetorno.Strings[i], 27, 1));
      end;

      if SameText(Copy(ArquivoRetorno.Strings[i], 1, 1), '1') then
      begin
        GNRERetorno.Identificador := StrToInt(Copy(ArquivoRetorno.Strings[i], 1, 1));
        GNRERetorno.SequencialGuia := StrToInt(Copy(ArquivoRetorno.Strings[i], 2, 4));
        GNRERetorno.SituacaoGuia := Trim(Copy(ArquivoRetorno.Strings[i], 6, 1));
        GNRERetorno.UFFavorecida := Trim(Copy(ArquivoRetorno.Strings[i], 7, 2));
        GNRERetorno.CodReceita := StrToInt(Copy(ArquivoRetorno.Strings[i], 9, 6));
        GNRERetorno.TipoDocEmitente := StrToInt(Copy(ArquivoRetorno.Strings[i], 15, 1));

        case GNRERetorno.TipoDocEmitente of
          1: GNRERetorno.DocEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 21, 11));
          2: GNRERetorno.DocEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 18, 14));
          3: GNRERetorno.DocEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 16, 16));
        end;

        GNRERetorno.RazaoSocialEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 32, 60));
        GNRERetorno.EnderecoEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 92, 60));
        GNRERetorno.MunicipioEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 152, 50));
        GNRERetorno.UFEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 202, 2));
        GNRERetorno.CEPEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 204, 8));
        GNRERetorno.TelefoneEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 212, 11));
        GNRERetorno.TipoDocDestinatario := StrToInt(Copy(ArquivoRetorno.Strings[i], 223, 1));

        case GNRERetorno.TipoDocDestinatario of
          1: GNRERetorno.DocDestinatario := Trim(Copy(ArquivoRetorno.Strings[i], 229, 11));
          2: GNRERetorno.DocDestinatario := Trim(Copy(ArquivoRetorno.Strings[i], 226, 14));
          3: GNRERetorno.DocDestinatario := Trim(Copy(ArquivoRetorno.Strings[i], 224, 16));
        end;

        GNRERetorno.MunicipioDestinatario := Trim(Copy(ArquivoRetorno.Strings[i], 240, 50));
        GNRERetorno.Produto := Trim(Copy(ArquivoRetorno.Strings[i], 290, 255));
        GNRERetorno.NumDocOrigem := Copy(ArquivoRetorno.Strings[i], 545, 18);
        GNRERetorno.Convenio := Trim(Copy(ArquivoRetorno.Strings[i], 563, 30));
        GNRERetorno.InfoComplementares := Trim(Copy(ArquivoRetorno.Strings[i], 593, 300));
        GNRERetorno.DataVencimento := Trim(Copy(ArquivoRetorno.Strings[i], 893, 8));
        GNRERetorno.DataLimitePagamento := Trim(Copy(ArquivoRetorno.Strings[i], 901, 8));
        GNRERetorno.PeriodoReferencia := Trim(Copy(ArquivoRetorno.Strings[i], 909, 1));
        GNRERetorno.MesAnoReferencia := Trim(Copy(ArquivoRetorno.Strings[i], 910, 6));
        GNRERetorno.Parcela := StrToInt(Copy(ArquivoRetorno.Strings[i], 916, 3));
        GNRERetorno.ValorPrincipal := StrToInt(Copy(ArquivoRetorno.Strings[i], 919, 15)) / 100;
        GNRERetorno.AtualizacaoMonetaria := StrToInt(Copy(ArquivoRetorno.Strings[i], 934, 15)) / 100;
        GNRERetorno.Juros := StrToInt(Copy(ArquivoRetorno.Strings[i], 949, 15)) / 100;
        GNRERetorno.Multa := StrToInt(Copy(ArquivoRetorno.Strings[i], 964, 15)) / 100;
        GNRERetorno.RepresentacaoNumerica := Copy(ArquivoRetorno.Strings[i], 979, 48);
        GNRERetorno.CodigoBarras := Copy(ArquivoRetorno.Strings[i], 1027, 44);
        GNRERetorno.QtdeVias := StrToInt(Copy(ArquivoRetorno.Strings[i], 1071, 1));
        GNRERetorno.NumeroControle := Copy(ArquivoRetorno.Strings[i], 1072, 16);
        GNRERetorno.IdentificadorGuia := Copy(ArquivoRetorno.Strings[i], 1088, 10);
        GNRERetorno.GuiaGeradaContingencia := StrToInt(Copy(ArquivoRetorno.Strings[i], 1098, 1));
        GNRERetorno.Reservado := Trim(Copy(ArquivoRetorno.Strings[i], 1099, 126));
      end
    end;
    
    ArquivoRetorno.Free;
  except
    raise;
  end;
end;

function TGuiasRetorno.LoadFromString(Arquivo: String): boolean;
var 
	GNRERetorno: TGNRERetorno;
  i: Integer;
	ArquivoRetorno: TStringList;
begin
  try
    Result := True;
		
		ArquivoRetorno := TStringList.Create;
		try
			ArquivoRetorno.Text := arquivo;
			
			GNRERetorno := TACBrGNRE(ACBrGNRE).GuiasRetorno.Add.GNRE;
			for i := 0 to ArquivoRetorno.Count - 1 do
			begin
				if SameText(Copy(ArquivoRetorno.Strings[i], 1, 1), '0') then
				begin
					GNRERetorno.InfoCabec.TipoIdentificadoSolicitante := StrToInt(Copy(ArquivoRetorno.Strings[i], 2, 1));
					GNRERetorno.InfoCabec.IdentificadorSolicitante := Trim(Copy(ArquivoRetorno.Strings[i], 3, 14));
					GNRERetorno.InfoCabec.NumeroProtocoloLote := Trim(Copy(ArquivoRetorno.Strings[i], 17, 10));
					GNRERetorno.InfoCabec.Ambiente := StrToInt(Copy(ArquivoRetorno.Strings[i], 27, 1));
				end;

				if SameText(Copy(ArquivoRetorno.Strings[i], 1, 1), '1') then
				begin
					GNRERetorno.Identificador := StrToInt(Copy(ArquivoRetorno.Strings[i], 1, 1));
					GNRERetorno.SequencialGuia := StrToInt(Copy(ArquivoRetorno.Strings[i], 2, 4));
					GNRERetorno.SituacaoGuia := Trim(Copy(ArquivoRetorno.Strings[i], 6, 1));
					GNRERetorno.UFFavorecida := Trim(Copy(ArquivoRetorno.Strings[i], 7, 2));
					GNRERetorno.CodReceita := StrToInt(Copy(ArquivoRetorno.Strings[i], 9, 6));
					GNRERetorno.TipoDocEmitente := StrToInt(Copy(ArquivoRetorno.Strings[i], 15, 1));

					case GNRERetorno.TipoDocEmitente of
						1: GNRERetorno.DocEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 21, 11));
						2: GNRERetorno.DocEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 18, 14));
						3: GNRERetorno.DocEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 16, 16));
					end;

					GNRERetorno.RazaoSocialEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 32, 60));
					GNRERetorno.EnderecoEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 92, 60));
					GNRERetorno.MunicipioEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 152, 50));
					GNRERetorno.UFEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 202, 2));
					GNRERetorno.CEPEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 204, 8));
					GNRERetorno.TelefoneEmitente := Trim(Copy(ArquivoRetorno.Strings[i], 212, 11));
					GNRERetorno.TipoDocDestinatario := StrToInt(Copy(ArquivoRetorno.Strings[i], 223, 1));

					case GNRERetorno.TipoDocDestinatario of
						1: GNRERetorno.DocDestinatario := Trim(Copy(ArquivoRetorno.Strings[i], 229, 11));
						2: GNRERetorno.DocDestinatario := Trim(Copy(ArquivoRetorno.Strings[i], 226, 14));
						3: GNRERetorno.DocDestinatario := Trim(Copy(ArquivoRetorno.Strings[i], 224, 16));
					end;

					GNRERetorno.MunicipioDestinatario := Trim(Copy(ArquivoRetorno.Strings[i], 240, 50));
					GNRERetorno.Produto := Trim(Copy(ArquivoRetorno.Strings[i], 290, 255));
					GNRERetorno.NumDocOrigem := Copy(ArquivoRetorno.Strings[i], 545, 18);
					GNRERetorno.Convenio := Trim(Copy(ArquivoRetorno.Strings[i], 563, 30));
					GNRERetorno.InfoComplementares := Trim(Copy(ArquivoRetorno.Strings[i], 593, 300));
					GNRERetorno.DataVencimento := Trim(Copy(ArquivoRetorno.Strings[i], 893, 8));
					GNRERetorno.DataLimitePagamento := Trim(Copy(ArquivoRetorno.Strings[i], 901, 8));
					GNRERetorno.PeriodoReferencia := Trim(Copy(ArquivoRetorno.Strings[i], 909, 1));
					GNRERetorno.MesAnoReferencia := Trim(Copy(ArquivoRetorno.Strings[i], 910, 6));
					GNRERetorno.Parcela := StrToInt(Copy(ArquivoRetorno.Strings[i], 916, 3));
					GNRERetorno.ValorPrincipal := StrToInt(Copy(ArquivoRetorno.Strings[i], 919, 15)) / 100;
					GNRERetorno.AtualizacaoMonetaria := StrToInt(Copy(ArquivoRetorno.Strings[i], 934, 15)) / 100;
					GNRERetorno.Juros := StrToInt(Copy(ArquivoRetorno.Strings[i], 949, 15)) / 100;
					GNRERetorno.Multa := StrToInt(Copy(ArquivoRetorno.Strings[i], 964, 15)) / 100;
					GNRERetorno.RepresentacaoNumerica := Copy(ArquivoRetorno.Strings[i], 979, 48);
					GNRERetorno.CodigoBarras := Copy(ArquivoRetorno.Strings[i], 1027, 44);
					GNRERetorno.QtdeVias := StrToInt(Copy(ArquivoRetorno.Strings[i], 1071, 1));
					GNRERetorno.NumeroControle := Copy(ArquivoRetorno.Strings[i], 1072, 16);
					GNRERetorno.IdentificadorGuia := Copy(ArquivoRetorno.Strings[i], 1088, 10);
					GNRERetorno.GuiaGeradaContingencia := StrToInt(Copy(ArquivoRetorno.Strings[i], 1098, 1));
					GNRERetorno.Reservado := Trim(Copy(ArquivoRetorno.Strings[i], 1099, 126));
				end
			end;
		finally
			ArquivoRetorno.free;
		end;
  except
    raise;
  end;
end;

procedure TGuiasRetorno.SetItem(Index: Integer; const Value: GuiaRetorno);
begin
  Items[Index].Assign(Value);
end;

end.
