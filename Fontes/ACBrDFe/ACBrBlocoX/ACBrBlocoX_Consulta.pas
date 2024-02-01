{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

{$I ACBr.inc}

unit ACBrBlocoX_Consulta;

interface

uses
  ACBrBlocoX_Comum, ACBrBase, Classes, SysUtils;

type

  { TACBrBlocoX_ConsultarProcessamentoArquivo }

  TACBrBlocoX_ConsultarProcessamentoArquivo = class(TACBrBlocoX_BaseFile)
  private
    FRecibo: AnsiString;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GerarXML(const Assinar: Boolean = True); override;
    procedure SaveToFile(const AXmlFileName: string; const AAssinar: Boolean = True); override;
    property Recibo: AnsiString read FRecibo write FRecibo;
  end;

  { TACBrBlocoX_ReprocessarArquivo }

  TACBrBlocoX_ReprocessarArquivo = class(TACBrBlocoX_BaseFile)
  private
    FRecibo: AnsiString;
    FMotivo: AnsiString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GerarXML(const Assinar: Boolean = True); override;
    procedure SaveToFile(const AXmlFileName: string; const AAssinar: Boolean = True); override;
    property Recibo: AnsiString read FRecibo write FRecibo;
    property Motivo: AnsiString read FMotivo write FMotivo;
  end;

  { TACBrBlocoX_DownloadArquivo }

  TACBrBlocoX_DownloadArquivo = class(TACBrBlocoX_BaseFile)
  private
    FRecibo: AnsiString;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GerarXML(const Assinar: Boolean = True); override;
    procedure SaveToFile(const AXmlFileName: string; const AAssinar: Boolean = True); override;
    property Recibo: AnsiString read FRecibo write FRecibo;
  end;

  { TACBrBlocoX_CancelarArquivo }

  TACBrBlocoX_CancelarArquivo = class(TACBrBlocoX_BaseFile)
  private
    FRecibo: AnsiString;
    FMotivo: AnsiString;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GerarXML(const Assinar: Boolean = True); override;
    procedure SaveToFile(const AXmlFileName: string; const AAssinar: Boolean = True); override;
    property Recibo: AnsiString read FRecibo write FRecibo;
    property Motivo: AnsiString read FMotivo write FMotivo;
  end;

  { TACBrBlocoX_ConsultarHistoricoArquivo }

  TACBrBlocoX_ConsultarHistoricoArquivo = class(TACBrBlocoX_BaseFile)
  private
    FRecibo: AnsiString;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GerarXML(const Assinar: Boolean = True); override;
    procedure SaveToFile(const AXmlFileName: string; const AAssinar: Boolean = True); override;
    property Recibo: AnsiString read FRecibo write FRecibo;
  end;

  { TACBrBlocoX_ListarArquivos }

  TACBrBlocoX_ListarArquivos = class(TACBrBlocoX_BaseFile)
  private
    FInscricaoEstadual: AnsiString;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GerarXML(const Assinar: Boolean = True); override;
    procedure SaveToFile(const AXmlFileName: string; const AAssinar: Boolean = True); override;
    property InscricaoEstadual: AnsiString read FInscricaoEstadual write FInscricaoEstadual;
  end;

  { TACBrBlocoX_ConsultarPendenciasContribuinte }

  TACBrBlocoX_ConsultarPendenciasContribuinte = class(TACBrBlocoX_BaseFile)
  private
    FInscricaoEstadual: AnsiString;
  public
    constructor Create(AOwner: TComponent); override;

    procedure GerarXML(const Assinar: Boolean = True); override;
    procedure SaveToFile(const AXmlFileName: string; const AAssinar: Boolean = True); override;

    property InscricaoEstadual: AnsiString read FInscricaoEstadual write FInscricaoEstadual;
  end;

  { TACBrBlocoX_ConsultarPendenciasDesenvolvedorPafEcf }

  TACBrBlocoX_ConsultarPendenciasDesenvolvedorPafEcf = class(TACBrBlocoX_BaseFile)
  private
    FCNPJ: AnsiString;
  public
    constructor Create(AOwner: TComponent); override;

    procedure GerarXML(const Assinar: Boolean = True); override;
    procedure SaveToFile(const AXmlFileName: string; const AAssinar: Boolean = True); override;

    property CNPJ: AnsiString read FCNPJ write FCNPJ;
  end;

implementation

uses
  ACBrBlocoX, ACBrUtil.XMLHTML, ACBrUtil.FilesIO,
  ACBrDFeConsts,
  pcnConversao, pcnGerador;

{ TACBrBlocoX_ConsultarProcessamentoArquivo }

constructor TACBrBlocoX_ConsultarProcessamentoArquivo.Create(AOwner: TComponent);
begin
  inherited;
  FRemoverEncodingXMLAssinado := True;
end;

procedure TACBrBlocoX_ConsultarProcessamentoArquivo.GerarXML(const Assinar: Boolean);
begin
  FXMLOriginal := '';
  FXMLAssinado := '';
  FGerador.ArquivoFormatoXML := '';
  FGerador.wGrupo(ENCODING_UTF8, '', False);
  FGerador.wGrupo('Manutencao Versao="1.0"');
  FGerador.wGrupo('Mensagem');
  FGerador.wCampo(tcStr, '', 'Recibo', 1, 36, 1, FRecibo);
  FGerador.wGrupo('/Mensagem');
  FGerador.wGrupo('/Manutencao');
  FXMLOriginal := ConverteXMLtoUTF8(FGerador.ArquivoFormatoXML);
  if Assinar then
    FXMLAssinado := TACBrBlocoX(FACBrBlocoX).SSL.Assinar(FXMLOriginal, 'Manutencao', 'Mensagem');
  if (FRemoverEncodingXMLAssinado) and
     (FXMLAssinado <> '') then
    FXMLAssinado := RemoverDeclaracaoXML(FXMLAssinado);
end;

procedure TACBrBlocoX_ConsultarProcessamentoArquivo.SaveToFile(const AXmlFileName: string; const AAssinar: Boolean);
begin
  GerarXML(AAssinar);
  if FXMLAssinado <> '' then
    WriteToTXT(AXmlFileName, FXMLAssinado, False, True)
  else
    WriteToTXT(AXmlFileName, FXMLOriginal, False, True);
end;

{ TACBrBlocoX_ConsultarHistoricoArquivo }
constructor TACBrBlocoX_ConsultarHistoricoArquivo.Create(AOwner: TComponent);
begin
  inherited;
  FRemoverEncodingXMLAssinado := True;
end;

procedure TACBrBlocoX_ConsultarHistoricoArquivo.GerarXML(const Assinar: Boolean);
begin
  FXMLOriginal := '';
  FXMLAssinado := '';
  FGerador.ArquivoFormatoXML := '';

  FGerador.wGrupo(ENCODING_UTF8, '', False);
  FGerador.wGrupo('ConsultarHistoricoArquivo Versao="1.0"');
  FGerador.wGrupo('Mensagem');

  FGerador.wCampo(tcStr, '', 'Recibo', 1, 36, 1, FRecibo);

  FGerador.wGrupo('/Mensagem');
  FGerador.wGrupo('/ConsultarHistoricoArquivo');

  FXMLOriginal := ConverteXMLtoUTF8(FGerador.ArquivoFormatoXML);
  if Assinar then
    FXMLAssinado := TACBrBlocoX(FACBrBlocoX).SSL.Assinar(FXMLOriginal, 'ConsultarHistoricoArquivo', 'Mensagem');
  if (FRemoverEncodingXMLAssinado) and
     (FXMLAssinado <> '') then
    FXMLAssinado := RemoverDeclaracaoXML(FXMLAssinado);
end;

procedure TACBrBlocoX_ConsultarHistoricoArquivo.SaveToFile(const AXmlFileName: string; const AAssinar: Boolean);
begin
  GerarXML(AAssinar);

  if FXMLAssinado <> '' then
    WriteToTXT(AXmlFileName, FXMLAssinado, False, True)
  else
    WriteToTXT(AXmlFileName, FXMLOriginal, False, True);
end;

{ TACBrBlocoX_ListarArquivos }
constructor TACBrBlocoX_ListarArquivos.Create(AOwner: TComponent);
begin
  inherited;
  FRemoverEncodingXMLAssinado := True;
end;

procedure TACBrBlocoX_ListarArquivos.GerarXML(const Assinar: Boolean);
begin
  FXMLOriginal := '';
  FXMLAssinado := '';
  FGerador.ArquivoFormatoXML := '';

  FGerador.wGrupo(ENCODING_UTF8, '', False);
  FGerador.wGrupo('ListarArquivos Versao="1.0"');
  FGerador.wGrupo('Mensagem');

  FGerador.wCampo(tcStr, '', 'IE', 1, 9, 1, FInscricaoEstadual);

  FGerador.wGrupo('/Mensagem');
  FGerador.wGrupo('/ListarArquivos');

  FXMLOriginal := ConverteXMLtoUTF8(FGerador.ArquivoFormatoXML);
  if Assinar then
    FXMLAssinado := TACBrBlocoX(FACBrBlocoX).SSL.Assinar(FXMLOriginal, 'ListarArquivos', 'Mensagem');
  if (FRemoverEncodingXMLAssinado) and
     (FXMLAssinado <> '') then
    FXMLAssinado := RemoverDeclaracaoXML(FXMLAssinado);
end;

procedure TACBrBlocoX_ListarArquivos.SaveToFile(const AXmlFileName: string; const AAssinar: Boolean);
begin
  GerarXML(AAssinar);

  if FXMLAssinado <> '' then
    WriteToTXT(AXmlFileName, FXMLAssinado, False, True)
  else
    WriteToTXT(AXmlFileName, FXMLOriginal, False, True);
end;

{ TACBrBlocoX_DownloadArquivo }
constructor TACBrBlocoX_DownloadArquivo.Create(AOwner: TComponent);
begin
  inherited;
  FRemoverEncodingXMLAssinado := True;
end;

procedure TACBrBlocoX_DownloadArquivo.GerarXML(const Assinar: Boolean);
begin
  FXMLOriginal := '';
  FXMLAssinado := '';
  FGerador.ArquivoFormatoXML := '';
  FGerador.wGrupo(ENCODING_UTF8, '', False);
  FGerador.wGrupo('DownloadArquivo Versao="1.0"');
  FGerador.wGrupo('Mensagem');
  FGerador.wCampo(tcStr, '', 'Recibo', 1, 36, 1, FRecibo);
  FGerador.wGrupo('/Mensagem');
  FGerador.wGrupo('/DownloadArquivo');
  FXMLOriginal := ConverteXMLtoUTF8(FGerador.ArquivoFormatoXML);
  if Assinar then
    FXMLAssinado := TACBrBlocoX(FACBrBlocoX).SSL.Assinar(FXMLOriginal, 'DownloadArquivo', 'Mensagem');
  if (FRemoverEncodingXMLAssinado) and
     (FXMLAssinado <> '') then
    FXMLAssinado := RemoverDeclaracaoXML(FXMLAssinado);
end;

procedure TACBrBlocoX_DownloadArquivo.SaveToFile(const AXmlFileName: string; const AAssinar: Boolean);
begin
  GerarXML(AAssinar);
  if FXMLAssinado <> '' then
    WriteToTXT(AXmlFileName, FXMLAssinado, False, True)
  else
    WriteToTXT(AXmlFileName, FXMLOriginal, False, True);
end;

{ TACBrBlocoX_CancelarArquivo }
constructor TACBrBlocoX_CancelarArquivo.Create(AOwner: TComponent);
begin
  inherited;
  FRemoverEncodingXMLAssinado := True;
end;

procedure TACBrBlocoX_CancelarArquivo.GerarXML(const Assinar: Boolean);
begin
  FXMLOriginal := '';
  FXMLAssinado := '';
  FGerador.ArquivoFormatoXML := '';
  FGerador.wGrupo(ENCODING_UTF8, '', False);
  FGerador.wGrupo('Manutencao Versao="1.0"');
  FGerador.wGrupo('Mensagem');
  FGerador.wCampo(tcStr, '', 'Recibo', 1, 36, 1, FRecibo);
  FGerador.wCampo(tcStr, '', 'Motivo', 1, 1000, 1, FMotivo);
  FGerador.wGrupo('/Mensagem');
  FGerador.wGrupo('/Manutencao');
  FXMLOriginal := ConverteXMLtoUTF8(FGerador.ArquivoFormatoXML);
  if Assinar then
    FXMLAssinado := TACBrBlocoX(FACBrBlocoX).SSL.Assinar(FXMLOriginal, 'Manutencao', 'Mensagem');
  if (FRemoverEncodingXMLAssinado) and
     (FXMLAssinado <> '') then
    FXMLAssinado := RemoverDeclaracaoXML(FXMLAssinado);
end;

procedure TACBrBlocoX_CancelarArquivo.SaveToFile(const AXmlFileName: string; const AAssinar: Boolean);
begin
  GerarXML(AAssinar);

  if FXMLAssinado <> '' then
    WriteToTXT(AXmlFileName, FXMLAssinado, False, True)
  else
    WriteToTXT(AXmlFileName, FXMLOriginal, False, True);
end;

{ TACBrBlocoX_ReprocessarArquivo }
constructor TACBrBlocoX_ReprocessarArquivo.Create(AOwner: TComponent);
begin
  inherited;
  FRemoverEncodingXMLAssinado := True;
end;

destructor TACBrBlocoX_ReprocessarArquivo.Destroy;
begin
  inherited;
end;

procedure TACBrBlocoX_ReprocessarArquivo.GerarXML(const Assinar: Boolean);
begin
  FXMLOriginal := '';
  FXMLAssinado := '';
  FGerador.ArquivoFormatoXML := '';
  FGerador.wGrupo(ENCODING_UTF8, '', False);
  FGerador.wGrupo('Manutencao Versao="1.0"');
  FGerador.wGrupo('Mensagem');
  FGerador.wCampo(tcStr, '', 'Recibo', 1, 36, 1, FRecibo);
  FGerador.wCampo(tcStr, '', 'Motivo', 1, 1000, 1, FMotivo);
  FGerador.wGrupo('/Mensagem');
  FGerador.wGrupo('/Manutencao');
  FXMLOriginal := ConverteXMLtoUTF8(FGerador.ArquivoFormatoXML);
  if Assinar then
    FXMLAssinado := TACBrBlocoX(FACBrBlocoX).SSL.Assinar(FXMLOriginal, 'Manutencao', 'Mensagem');
  if (FRemoverEncodingXMLAssinado) and
     (FXMLAssinado <> '') then
    FXMLAssinado := RemoverDeclaracaoXML(FXMLAssinado);
end;

procedure TACBrBlocoX_ReprocessarArquivo.SaveToFile(const AXmlFileName: string; const AAssinar: Boolean);
begin
  GerarXML(AAssinar);
  if FXMLAssinado <> '' then
    WriteToTXT(AXmlFileName, FXMLAssinado, False, True)
  else
    WriteToTXT(AXmlFileName, FXMLOriginal, False, True);
end;

{ TACBrBlocoX_ConsultarPendenciasContribuinte }
constructor TACBrBlocoX_ConsultarPendenciasContribuinte.Create(AOwner: TComponent);
begin
  inherited;
  FRemoverEncodingXMLAssinado := True;
end;

procedure TACBrBlocoX_ConsultarPendenciasContribuinte.GerarXML(const Assinar: Boolean);
begin
  FXMLOriginal := '';
  FXMLAssinado := '';
  FGerador.ArquivoFormatoXML := '';

  FGerador.wGrupo(ENCODING_UTF8, '', False);
  FGerador.wGrupo('ConsultarPendenciasContribuinte Versao="1.0"');
  FGerador.wGrupo('Mensagem');

  FGerador.wCampo(tcStr, '', 'IE', 1, 9, 1, FInscricaoEstadual);

  FGerador.wGrupo('/Mensagem');
  FGerador.wGrupo('/ConsultarPendenciasContribuinte');

  FXMLOriginal := ConverteXMLtoUTF8(FGerador.ArquivoFormatoXML);
  if Assinar then
    FXMLAssinado := TACBrBlocoX(FACBrBlocoX).SSL.Assinar(FXMLOriginal, 'ConsultarPendenciasContribuinte', 'Mensagem');
  if (FRemoverEncodingXMLAssinado) and
     (FXMLAssinado <> '') then
    FXMLAssinado := RemoverDeclaracaoXML(FXMLAssinado);
end;

procedure TACBrBlocoX_ConsultarPendenciasContribuinte.SaveToFile(const AXmlFileName: string; const AAssinar: Boolean);
begin
  GerarXML(AAssinar);

  if FXMLAssinado <> '' then
    WriteToTXT(AXmlFileName, FXMLAssinado, False, True)
  else
    WriteToTXT(AXmlFileName, FXMLOriginal, False, True);
end;

{ TACBrBlocoX_ConsultarPendenciasDesenvolvedorPafEcf }
constructor TACBrBlocoX_ConsultarPendenciasDesenvolvedorPafEcf.Create(AOwner: TComponent);
begin
  inherited;
  FRemoverEncodingXMLAssinado := True;
end;

procedure TACBrBlocoX_ConsultarPendenciasDesenvolvedorPafEcf.GerarXML(const Assinar: Boolean);
begin
  FXMLOriginal := '';
  FXMLAssinado := '';
  FGerador.ArquivoFormatoXML := '';

  FGerador.wGrupo(ENCODING_UTF8, '', False);
  FGerador.wGrupo('ConsultarPendenciasDesenvolvedorPafEcf Versao="1.0"');
  FGerador.wGrupo('Mensagem');

  FGerador.wCampo(tcStr, '', 'CNPJ', 1, 14, 1, FCNPJ);

  FGerador.wGrupo('/Mensagem');
  FGerador.wGrupo('/ConsultarPendenciasDesenvolvedorPafEcf');

  FXMLOriginal := ConverteXMLtoUTF8(FGerador.ArquivoFormatoXML);
  if Assinar then
    FXMLAssinado := TACBrBlocoX(FACBrBlocoX).SSL.Assinar(FXMLOriginal, 'ConsultarPendenciasDesenvolvedorPafEcf', 'Mensagem');
  if (FRemoverEncodingXMLAssinado) and
     (FXMLAssinado <> '') then
    FXMLAssinado := RemoverDeclaracaoXML(FXMLAssinado);
end;

procedure TACBrBlocoX_ConsultarPendenciasDesenvolvedorPafEcf.SaveToFile(const AXmlFileName: string; const AAssinar: Boolean);
begin
  GerarXML(AAssinar);

  if FXMLAssinado <> '' then
    WriteToTXT(AXmlFileName, FXMLAssinado, False, True)
  else
    WriteToTXT(AXmlFileName, FXMLOriginal, False, True);
end;

end.

