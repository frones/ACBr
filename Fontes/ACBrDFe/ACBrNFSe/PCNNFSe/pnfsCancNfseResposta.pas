{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{$I ACBr.inc}

unit pnfsCancNfseResposta;

interface

uses
  SysUtils, Classes, Forms, 
  pcnAuxiliar, pcnConversao, pcnLeitor,
  pnfsConversao, pnfsNFSe;

type

 TMsgRetornoCancCollection = class;
 TMsgRetornoCancCollectionItem = class;
 TNotasCanceladasCollection = class;
 TNotasCanceladasCollectionItem = class;

 TInfCanc = class(TPersistent)
  private
    FPedido: TPedidoCancelamento;
    FDataHora: TDateTime;
    FConfirmacao : string;  // Alterado por Nilton Olher - 20/02/2015
    FSucesso: String;
    FMsgCanc: String;
    FMsgRetorno : TMsgRetornoCancCollection;
    FNotasCanceladas: TNotasCanceladasCollection;

    procedure SetMsgRetorno(Value: TMsgRetornoCancCollection);
    procedure SetNotasCanceladas(const Value: TNotasCanceladasCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property Pedido: TPedidocancelamento           read FPedido      write FPedido;
    property DataHora: TDateTime                   read FDataHora    write FDataHora;
    property Confirmacao: String                   read FConfirmacao write FConfirmacao;  // Alterado por Nilton Olher - 20/02/2015
    property Sucesso: String                       read FSucesso     write FSucesso;
    property MsgCanc: String                       read FMsgCanc     write FMsgCanc;
    property MsgRetorno: TMsgRetornoCancCollection read FMsgRetorno write SetMsgRetorno;
    property NotasCanceladas: TNotasCanceladasCollection read FNotasCanceladas write SetNotasCanceladas;
  end;

 TMsgRetornoCancCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TMsgRetornoCancCollectionItem;
    procedure SetItem(Index: Integer; Value: TMsgRetornoCancCollectionItem);
  public
    constructor Create(AOwner: TInfCanc);
    function Add: TMsgRetornoCancCollectionItem;
    property Items[Index: Integer]: TMsgRetornoCancCollectionItem read GetItem write SetItem; default;
  end;

 TMsgRetornoCancCollectionItem = class(TCollectionItem)
  private
    FCodigo : String;
    FMensagem : String;
    FCorrecao : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property Codigo: string   read FCodigo   write FCodigo;
    property Mensagem: string read FMensagem write FMensagem;
    property Correcao: string read FCorrecao write FCorrecao;
  end;

 TNotasCanceladasCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TNotasCanceladasCollectionItem;
    procedure SetItem(Index: Integer; Value: TNotasCanceladasCollectionItem);
  public
    constructor Create(AOwner: TInfCanc);
    function Add: TNotasCanceladasCollectionItem;
    property Items[Index: Integer]: TNotasCanceladasCollectionItem read GetItem write SetItem; default;
  end;

 TNotasCanceladasCollectionItem = class(TCollectionItem)
  private
    FNumeroNota : String;
    FCodigoVerficacao : String;
    FInscricaoMunicipalPrestador : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property NumeroNota: string read FNumeroNota  write FNumeroNota;
    property CodigoVerficacao: string read FCodigoVerficacao write FCodigoVerficacao;
    property InscricaoMunicipalPrestador: string read FInscricaoMunicipalPrestador write FInscricaoMunicipalPrestador;
  end;

 TretCancNFSe = class(TPersistent)
  private
    FLeitor: TLeitor;
    FInfCanc: TInfCanc;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
    function LerXml_provedorIssDsf: boolean;
    function LerXml_provedorInfisc(VersaoXML: string = '1'): Boolean;
    function LerXML_provedorEquiplano: Boolean;    
    function LerXml_provedorNFSEBrasil: boolean;
	
  published
    property Leitor: TLeitor   read FLeitor   write FLeitor;
    property InfCanc: TInfCanc read FInfCanc  write FInfCanc;
  end;

implementation

{ TInfCanc }

constructor TInfCanc.Create;
begin
  FPedido     := TPedidoCancelamento.Create;
  FMsgRetorno := TMsgRetornoCancCollection.Create(Self);
  FNotasCanceladas := TNotasCanceladasCollection.Create(Self);
end;

destructor TInfCanc.Destroy;
begin
  FPedido.Free;
  FMsgRetorno.Free;
  FNotasCanceladas.Free;

  inherited;
end;

procedure TInfCanc.SetMsgRetorno(Value: TMsgRetornoCancCollection);
begin
  FMsgRetorno.Assign(Value);
end;

procedure TInfCanc.SetNotasCanceladas(const Value: TNotasCanceladasCollection);
begin
  FNotasCanceladas.Assign(Value);
end;

{ TMsgRetornoCancCollection }

function TMsgRetornoCancCollection.Add: TMsgRetornoCancCollectionItem;
begin
  Result := TMsgRetornoCancCollectionItem(inherited Add);
  Result.create;
end;

constructor TMsgRetornoCancCollection.Create(AOwner: TInfCanc);
begin
  inherited Create(TMsgRetornoCancCollectionItem);
end;

function TMsgRetornoCancCollection.GetItem(
  Index: Integer): TMsgRetornoCancCollectionItem;
begin
  Result := TMsgRetornoCancCollectionItem(inherited GetItem(Index));
end;

procedure TMsgRetornoCancCollection.SetItem(Index: Integer;
  Value: TMsgRetornoCancCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TMsgRetornoCancCollectionItem }

constructor TMsgRetornoCancCollectionItem.Create;
begin

end;

destructor TMsgRetornoCancCollectionItem.Destroy;
begin

  inherited;
end;

{ TNotasCanceladasCollection }

function TNotasCanceladasCollection.Add: TNotasCanceladasCollectionItem;
begin
  Result := TNotasCanceladasCollectionItem(inherited Add);
  Result.create;
end;

constructor TNotasCanceladasCollection.Create(AOwner: TInfCanc);
begin
  inherited Create(TNotasCanceladasCollectionItem);
end;

function TNotasCanceladasCollection.GetItem(
  Index: Integer): TNotasCanceladasCollectionItem;
begin
  Result := TNotasCanceladasCollectionItem(inherited GetItem(Index));
end;

procedure TNotasCanceladasCollection.SetItem(Index: Integer;
  Value: TNotasCanceladasCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TNotasCanceladasCollectionItem }

constructor TNotasCanceladasCollectionItem.Create;
begin

end;

destructor TNotasCanceladasCollectionItem.Destroy;
begin

  inherited;
end;

{ TretCancNFSe }

constructor TretCancNFSe.Create;
begin
  FLeitor  := TLeitor.Create;
  FInfCanc := TInfCanc.Create;
end;

destructor TretCancNFSe.Destroy;
begin
  FLeitor.Free;
  FInfCanc.Free;
  inherited;
end;

function TretCancNFSe.LerXml: boolean;
var
  i: Integer;
begin
  result := True;

  try
    // Incluido por Ricardo Miranda em 14/03/2013
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    Leitor.Grupo   := Leitor.Arquivo;

  { Incluído por Márcio Teixeira em 14/02/2013 para tratar os retornos do Ginfes.
    Fiz seguindo a seguinte idéia: se infCanc.DataHora tiver data, então foi
    cancelado com sucesso, caso contrário houve algum problema.
  }
  if Pos('www.ginfes.com.br', Leitor.Arquivo) <> 0
   then begin
    if (leitor.rExtrai(1, 'CancelarNfseResposta') <> '')
     then begin
      if AnsiLowerCase(Leitor.rCampo(tcStr, 'Sucesso')) = 'true'
       then begin
         infCanc.DataHora := Leitor.rCampo(tcDatHor, 'DataHora');
         InfCanc.Sucesso  := Leitor.rCampo(tcStr,    'Sucesso');  //Incluido por jrJunior82 09/05/2013
         InfCanc.MsgCanc  := Leitor.rCampo(tcStr,    'Mensagem'); //Incluido por jrJunior82 09/05/2013
       end
       else infCanc.DataHora := 0;

      InfCanc.FPedido.InfID.ID           := '';
      InfCanc.FPedido.CodigoCancelamento := '';

      if Leitor.rExtrai(1, 'MensagemRetorno') <> ''
       then
       if Pos('cancelada com sucesso', AnsiLowerCase(Leitor.rCampo(tcStr, 'Mensagem'))) = 0
        then begin
        InfCanc.FMsgRetorno.Add;
        InfCanc.FMsgRetorno[0].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        InfCanc.FMsgRetorno[0].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
        InfCanc.FMsgRetorno[0].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');
        end;
      end;

    end
  else
    begin

      // Alterado por Akai - L. Massao Aihara 31/10/2013
      if (leitor.rExtrai(1, 'CancelarNfseResposta') <> '') or
         (leitor.rExtrai(1, 'Cancelarnfseresposta') <> '') or
         (leitor.rExtrai(1, 'CancelarNfseReposta') <> '') or
         (leitor.rExtrai(1, 'CancelarNfseResult') <> '') then
      begin
        infCanc.DataHora := Leitor.rCampo(tcDatHor, 'DataHora');
        if infCanc.DataHora = 0 then
          infCanc.DataHora := Leitor.rCampo(tcDatHor, 'DataHoraCancelamento');
        InfCanc.FConfirmacao := Leitor.rAtributo('Confirmacao Id=');    // Alterado por Nilton Olher - 20/02/2015
        InfCanc.Sucesso  := Leitor.rCampo(tcStr,    'Sucesso');  // Incluido por Glecio 09/05/2013

        InfCanc.FPedido.InfID.ID := Leitor.rAtributo('InfPedidoCancelamento Id=');
        if InfCanc.FPedido.InfID.ID = '' then
          InfCanc.FPedido.InfID.ID := Leitor.rAtributo('InfPedidoCancelamento id=');

        InfCanc.FPedido.CodigoCancelamento := Leitor.rCampo(tcStr, 'CodigoCancelamento');


        if Leitor.rExtrai(2, 'IdentificacaoNfse') <> ''
         then begin
          InfCanc.FPedido.IdentificacaoNfse.Numero             := Leitor.rCampo(tcStr, 'Numero');
          InfCanc.FPedido.IdentificacaoNfse.Cnpj               := Leitor.rCampo(tcStr, 'Cnpj');
          InfCanc.FPedido.IdentificacaoNfse.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
          InfCanc.FPedido.IdentificacaoNfse.CodigoMunicipio    := Leitor.rCampo(tcStr, 'CodigoMunicipio');
         end;

        Leitor.Grupo := Leitor.Arquivo;

        InfCanc.FPedido.signature.URI             := Leitor.rAtributo('Reference URI=');
        InfCanc.FPedido.signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
        InfCanc.FPedido.signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
        InfCanc.FPedido.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');

        if (leitor.rExtrai(2, 'ListaMensagemRetorno') <> '') then
        begin
          i := 0;
          while Leitor.rExtrai(3, 'MensagemRetorno', '', i + 1) <> '' do
           begin
                InfCanc.FMsgRetorno.Add;
                InfCanc.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
                InfCanc.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
                InfCanc.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');

                inc(i);
            end;
        end;

        if (leitor.rExtrai(1, 'ListaMensagemRetorno') <> '') then begin    // Modificado para o Provedor Freire
           InfCanc.FMsgRetorno.Add;
           InfCanc.FMsgRetorno[0].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
           InfCanc.FMsgRetorno[0].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
           InfCanc.FMsgRetorno[0].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');
        end;

      end;
    end;

    i := 0;
    while (Leitor.rExtrai(1, 'Fault', '', i + 1) <> '') do
     begin
       InfCanc.FMsgRetorno.Add;
       InfCanc.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'faultcode');
       InfCanc.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'faultstring');
       InfCanc.FMsgRetorno[i].FCorrecao := '';

       inc(i);
     end;

  except
    result := False;
  end;
end;

function TretCancNFSe.LerXml_provedorIssDsf: boolean; //falta homologar
var
  i{, posI, count}: Integer;
  VersaoXML: String;
//  strAux: AnsiString;
//  leitorAux: TLeitor;
begin
  result := False;

  try
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    VersaoXML      := '1';
    Leitor.Grupo   := Leitor.Arquivo;

    if leitor.rExtrai(1, 'RetornoCancelamentoNFSe') <> '' then
    begin

      if (leitor.rExtrai(2, 'Cabecalho') <> '') then
      begin
         FInfCanc.FSucesso := Leitor.rCampo(tcStr, 'Sucesso');
      end;

      i := 0 ;
      if (leitor.rExtrai(2, 'NotasCanceladas') <> '') then
      begin
        while (Leitor.rExtrai(2, 'Nota', '', i + 1) <> '') do
        begin
          FInfCanc.FNotasCanceladas.Add;
          FInfCanc.FNotasCanceladas[i].InscricaoMunicipalPrestador := Leitor.rCampo(tcStr, 'InscricaoMunicipalPrestador');
          FInfCanc.FNotasCanceladas[i].NumeroNota                  := Leitor.rCampo(tcStr, 'NumeroNota');
          FInfCanc.FNotasCanceladas[i].CodigoVerficacao            := Leitor.rCampo(tcStr,'CodigoVerificacao');
          inc(i);
        end;
      end;

      i := 0 ;
      if (leitor.rExtrai(2, 'Alertas') <> '') then
      begin
        while (Leitor.rExtrai(2, 'Alerta', '', i + 1) <> '') do
        begin
          InfCanc.FMsgRetorno.Add;
          InfCanc.FMsgRetorno[i].FCodigo  := Leitor.rCampo(tcStr, 'Codigo');
          InfCanc.FMsgRetorno[i].Mensagem := Leitor.rCampo(tcStr, 'Descricao');
          inc(i);
        end;
      end;

      i := 0 ;
      if (leitor.rExtrai(2, 'Erros') <> '') then
      begin
        while (Leitor.rExtrai(2, 'Erro', '', i + 1) <> '') do
        begin
          InfCanc.FMsgRetorno.Add;
          InfCanc.FMsgRetorno[i].FCodigo  := Leitor.rCampo(tcStr, 'Codigo');
          InfCanc.FMsgRetorno[i].Mensagem := Leitor.rCampo(tcStr, 'Descricao');
          inc(i);
        end;
      end;

      Result := True;
    end;
  except
    result := False;
  end;
end;

function TretCancNFSe.LerXml_provedorInfisc(VersaoXML: string = '1'): boolean;
var
  sMotCod,sMotDes,sCancelaAnula: string;
begin
  Result := False;
  try
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    Leitor.Grupo   := Leitor.Arquivo;
    // Alterado por Moro em 27/02/2015
    // Trocado resAnulaNFSe por resCancelaNFSe
    if VersaoXML = '1.1' then
      sCancelaAnula := 'resCancelaNFSe' // Caxias do Sul Versão XML 1.1
    else
      sCancelaAnula := 'resAnulaNFSe';  // Demais Cidades

    if leitor.rExtrai(1, sCancelaAnula) <> '' then
    //if leitor.rExtrai(1, 'resAnulaNFSe') <> '' then
    begin
      InfCanc.FSucesso := Leitor.rCampo(tcStr, 'sit');
      if (InfCanc.FSucesso = '100') then // 100-Aceito
      begin
         InfCanc.FPedido.IdentificacaoNfse.Cnpj := Leitor.rCampo(tcStr, 'CNPJ');
         InfCanc.FPedido.IdentificacaoNfse.Numero := Leitor.rCampo(tcStr, 'chvAcessoNFS-e');
         InfCanc.FDataHora := Leitor.rCampo(tcDatHor, 'dhRecbto');
      end
      else if (InfCanc.FSucesso = '200') then // 200-Rejeitado
      begin
        sMotDes:=Leitor.rCampo(tcStr, 'mot');
        if Pos('Error',sMotDes)>0 then
          sMotCod:=SomenteNumeros(copy(sMotDes,1,Pos(' ',sMotDes)))
        else
          sMotCod:='';
        InfCanc.MsgRetorno.Add;
        InfCanc.MsgRetorno[0].FCodigo   := sMotCod;
        InfCanc.MsgRetorno[0].FMensagem := sMotDes+' '+
                                          'CNPJ '+Leitor.rCampo(tcStr, 'CNPJ')+' '+
                                          'DATA '+Leitor.rCampo(tcStr, 'dhRecbto');
        InfCanc.MsgRetorno[0].FCorrecao := '';
      end;
      Result := True;
    end;
  except
    Result := False;
  end;
end;

function TretCancNFSe.LerXML_provedorEquiplano: Boolean;
var
  i: Integer;
begin
  try
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    Leitor.Grupo   := Leitor.Arquivo;

    InfCanc.FSucesso := Leitor.rCampo(tcStr, 'Sucesso');
    InfCanc.FDataHora:= Leitor.rCampo(tcDatHor, 'dtCancelamento');

    if leitor.rExtrai(1, 'mensagemRetorno') <> '' then
      begin
        i := 0;
        if (leitor.rExtrai(2, 'listaErros') <> '') then
          begin
            while Leitor.rExtrai(3, 'erro', '', i + 1) <> '' do
              begin
                InfCanc.FMsgRetorno.Add;
                InfCanc.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'cdMensagem');
                InfCanc.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'dsMensagem');
                InfCanc.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'dsCorrecao');

                inc(i);
              end;
          end;

        if (leitor.rExtrai(2, 'listaAlertas') <> '') then
          begin
            while Leitor.rExtrai(3, 'alerta', '', i + 1) <> '' do
              begin
                InfCanc.FMsgRetorno.Add;
                InfCanc.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'cdMensagem');
                InfCanc.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'dsMensagem');
                InfCanc.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'dsCorrecao');

                inc(i);
              end;
          end;
      end;

    Result := True;
  except
    result := False;
  end;

end;

// Luiz Baião 2014.12.09 
function TretCancNFSe.LerXml_provedorNFSEBrasil: boolean;
var
  ok: boolean;
  i, Item, posI, count: Integer;
  VersaoXML: String;
  strAux,strAux2, strItem: AnsiString;
  leitorAux, leitorItem:TLeitor;
begin
  result := False;
  (*
   // Luiz Baião 2014.12.03 
  try
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    VersaoXML      := '1';
    Leitor.Grupo   := Leitor.Arquivo;

    strAux := leitor.rExtrai_NFSEBrasil(1, 'RespostaLoteRps');

    strAux := leitor.rExtrai_NFSEBrasil(1, 'erros');
    if ( strAux <> emptystr) then begin
    
        posI := 1;
        i := 0 ;
        while ( posI > 0 ) do begin
             count := pos('</erro>', strAux) + 7;

             LeitorAux := TLeitor.Create;
             leitorAux.Arquivo := copy(strAux, PosI, count);
             leitorAux.Grupo   := leitorAux.Arquivo;
             strAux2 := leitorAux.rExtrai_NFSEBrasil(1,'erro');
             strAux2 := Leitor.rCampo(tcStr, 'erro');
             InfCanc.FMsgRetorno.Add;
             InfCanc.FMsgRetorno.Items[i].Mensagem := Leitor.rCampo(tcStr, 'erro')+#13;
             inc(i);
             LeitorAux.free;
             Delete(strAux, PosI, count);
             posI := pos('<erro>', strAux);
        end;
    end;

    strAux := leitor.rExtrai_NFSEBrasil(1, 'confirmacoes');
    if ( strAux <> emptystr) then begin

        posI := 1;
        // i := 0 ;
        while ( posI > 0 ) do begin
        
           count := pos('</confirmacao>', strAux) + 7;
           LeitorAux := TLeitor.Create;
           leitorAux.Arquivo := copy(strAux, PosI, count);
           leitorAux.Grupo   := leitorAux.Arquivo;
           strAux2 := leitorAux.rExtrai_NFSEBrasil(1,'confirmacao');
           strAux2 := Leitor.rCampo(tcStr, 'confirmacao');
           InfCanc.FMsgRetorno.Add;
           InfCanc.FMsgRetorno.Items[i].Mensagem := Leitor.rCampo(tcStr, 'confirmacao')+#13;
           inc(i);
           LeitorAux.free;
           Delete(strAux, PosI, count);
           posI := pos('<confirmacao>', strAux);
        end;
    end;

    Result := True;
  except
    result := False;
  end;
  *)
end;

end.

