package com.acbr.nfe.acbrlibnfe.demo.comandos;

import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;

import com.acbr.nfe.acbrlibnfe.demo.utils.ACBrLibHelper;
import com.acbr.nfe.acbrlibnfe.demo.R;

import br.com.acbr.lib.nfe.ACBrLibNFe;

public class ComandosEnvioNFeFragment extends Fragment {

    private ACBrLibNFe ACBrNFe;

    private EditText txtNFeINI;
    private EditText txtNFeXML;
    private EditText txtRespostaEnvio;
    private EditText txtDestinatario;
    private Button btnLimparLista;
    private Button btnEnviarNFe;
    private Button btnImprimirNFCe;
    private Button btnImprimirPDFNFCe;
    private Button btnLimparRespostaEnvio;
    private Button btnEnviarEmailNFCe;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_envio_nfe, container, false);

        ACBrNFe = ACBrLibHelper.getInstance("ACBrLib.ini");

        checkAndRequestBluetoothPermission();

        txtNFeINI = view.findViewById(R.id.txtNFeINI);
        txtNFeXML = view.findViewById(R.id.txtNFeXML);
        txtDestinatario = view.findViewById(R.id.txtDestinatario);
        txtRespostaEnvio = view.findViewById(R.id.txtRespostaEnvio);
        btnLimparLista = view.findViewById(R.id.btnLimparLista);
        btnEnviarNFe = view.findViewById(R.id.btnEnviarNFe);
        btnEnviarEmailNFCe = view.findViewById(R.id.btnEnviarEmailNFCe);
        btnImprimirNFCe = view.findViewById(R.id.btnImprimirNFCe);
        btnImprimirPDFNFCe = view.findViewById(R.id.btnImprimirPDFNFCe);
        btnLimparRespostaEnvio = view.findViewById(R.id.btnLimparRespostaEnvio);

        btnLimparLista.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                limparListaNFe();
            }
        });

        btnEnviarNFe.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                enviarNFe();
            }
        });

        btnEnviarEmailNFCe.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                enviarEmailNFCe();
            }
        });

        btnImprimirNFCe.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                imprimirNFCe();
            }
        });

        btnImprimirPDFNFCe.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                imprimirPDFNFCe();
            }
        });


        btnLimparRespostaEnvio.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaEnvio();
            }
        });

        return view;
    }

    public void checkAndRequestBluetoothPermission() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            if (requireContext().checkSelfPermission(android.Manifest.permission.BLUETOOTH_CONNECT) != PackageManager.PERMISSION_GRANTED ||
                    requireContext().checkSelfPermission(android.Manifest.permission.BLUETOOTH_SCAN) != PackageManager.PERMISSION_GRANTED) {
                requestPermissions(new String[]{
                        android.Manifest.permission.BLUETOOTH_CONNECT,
                        android.Manifest.permission.BLUETOOTH_SCAN
                }, 1001);
            }
        } else {
            if (requireContext().checkSelfPermission(android.Manifest.permission.BLUETOOTH) != PackageManager.PERMISSION_GRANTED ||
                    requireContext().checkSelfPermission(android.Manifest.permission.BLUETOOTH_ADMIN) != PackageManager.PERMISSION_GRANTED) {
                requestPermissions(new String[]{
                        android.Manifest.permission.BLUETOOTH,
                        android.Manifest.permission.BLUETOOTH_ADMIN
                }, 1001);
            }
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);

        if (requestCode == 1001) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                Log.d("Permissions", "Permissões de Bluetooth concedidas.");
                // Se você tem código que depende da permissão, pode chamá-lo aqui
            } else {
                Log.d("Permissions", "Permissões de Bluetooth negadas.");
            }
        }
    }

    public void limparListaNFe() {
        txtRespostaEnvio.setText("");
        try {
            ACBrNFe.LimparLista();
            txtRespostaEnvio.setText("Método executado com sucesso !!");
        } catch (Exception ex) {
            Log.e("Erro ao Limpar Lista NFe", ex.getMessage());
            txtRespostaEnvio.setText(ex.getMessage());
        }
    }

    public void enviarNFe() {
        txtRespostaEnvio.setText("");
        String result = "";
        String NFeIni = txtNFeINI.getText().toString();
        try {
            ACBrNFe.CarregarINI(NFeIni);
            result = ACBrNFe.Enviar(1, false, false, false);
        } catch (Exception ex) {
            Log.e("Erro ao Enviar NFe", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaEnvio.setText(result);
        }
    }

    public void enviarEmailNFCe() {
        txtRespostaEnvio.setText("");
        String NFeXml = txtNFeXML.getText().toString();
        String destinatario = txtDestinatario.getText().toString();
        try {
            ACBrNFe.EnviarEmail(destinatario, NFeXml, true, "Envio NFCe Email", "", "", "Envio NFCe");
            txtRespostaEnvio.setText("Email enviado com sucesso!");
        } catch (Exception ex) {
            Log.e("Erro ao Enviar Email", ex.getMessage());
            txtRespostaEnvio.setText(ex.getMessage());
        }
    }

    public void imprimirNFCe() {
        // Verifica se as permissões necessárias foram concedidas
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            if (requireContext().checkSelfPermission(android.Manifest.permission.BLUETOOTH_CONNECT) != PackageManager.PERMISSION_GRANTED ||
                    requireContext().checkSelfPermission(android.Manifest.permission.BLUETOOTH_SCAN) != PackageManager.PERMISSION_GRANTED) {
                Log.d("Permissions", "Permissões de Bluetooth não concedidas.");
                return; // Não continua se a permissão não foi concedida
            }
        } else {
            if (requireContext().checkSelfPermission(android.Manifest.permission.BLUETOOTH) != PackageManager.PERMISSION_GRANTED ||
                    requireContext().checkSelfPermission(android.Manifest.permission.BLUETOOTH_ADMIN) != PackageManager.PERMISSION_GRANTED) {
                Log.d("Permissions", "Permissões de Bluetooth não concedidas.");
                return; // Não continua se a permissão não foi concedida
            }
        }

        try {
            Log.d("Print", "Limpando lista antes da impressão.");
            ACBrNFe.LimparLista();

            String xmlContent = txtNFeXML.getText().toString();
            if (xmlContent.isEmpty()) {
                Log.d("Print", "XML está vazio. Não pode carregar.");
                return;
            }

            Log.d("Print", "Carregando XML: " + xmlContent);
            ACBrNFe.CarregarXML(xmlContent);
            Log.d("Print", "Preparando para imprimir.");
            ACBrNFe.Imprimir("", 1, "False", "False", "False", "False", "False");
            Log.d("Print", "Impressão realizada com sucesso.");
        } catch (Exception ex) {
            Log.e("Erro ao Imprimir NFCe", ex.getMessage());
            ex.printStackTrace();  // Para ver a pilha de erros
            txtRespostaEnvio.setText(ex.getMessage());
        }
    }

    public void imprimirPDFNFCe(){
        try {
            Log.d("Print", "Limpando lista antes da impressão.");
            ACBrNFe.LimparLista();

            String xmlContent = txtNFeXML.getText().toString();
            if (xmlContent.isEmpty()) {
                Log.d("Print", "XML está vazio. Não pode carregar.");
                return;
            }

            Log.d("Print", "Carregando XML: " + xmlContent);
            ACBrNFe.CarregarXML(xmlContent);
            Log.d("Print", "Preparando para imprimir.");
            ACBrNFe.ImprimirPDF();
            Log.d("Print", "Impressão PDF realizada com sucesso.");
        } catch (Exception ex) {
            Log.e("Erro ao Imprimir NFCe", ex.getMessage());
            ex.printStackTrace();  // Para ver a pilha de erros
            txtRespostaEnvio.setText(ex.getMessage());
        }
    }

    public void LimparRespostaEnvio() {
        txtRespostaEnvio.setText("");
    }
}