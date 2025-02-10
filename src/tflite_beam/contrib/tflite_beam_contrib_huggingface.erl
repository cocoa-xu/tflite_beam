-module(tflite_beam_contrib_huggingface).
-export([all_models/0, model_urls/1, download_model/1]).

-define(HUGGINGFACE_BASEURL, "https://huggingface.co/").

repo_download_baseurl(Repo, Branch) -> 
    ?HUGGINGFACE_BASEURL ++ Repo ++ "/resolve/" ++ Branch.

model_urls(#{repo := Repo, files := Files}) ->
    RepoBaseURL = repo_download_baseurl(Repo, "main"),
    lists:map(fun(File) -> {Repo, File, RepoBaseURL ++ "/" ++ File ++ "?download=true"} end, Files).

download_model(ModelInfo) ->
    ModelURLs = model_urls(ModelInfo),
    lists:foldl(fun({Repo, File, URL}, Status) ->
        case Status of
            {ok, Acc} ->
                case tflite_beam_utils_downloader:download(URL, Repo, File, false) of 
                    {ok, DestionationFilename} ->
                        {ok, [DestionationFilename | Acc]};
                    {error, Reason} ->
                        {error, Reason}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    end, {ok, []}, ModelURLs).

all_models() -> 
    [
        #{repo => "qualcomm/ConvNext-Tiny", files => ["ConvNext-Tiny.tflite"], task => image_classification},
        #{repo => "qualcomm/DenseNet-121", files => ["DenseNet-121.tflite"], task => image_classification},
        #{repo => "qualcomm/EfficientNet-B0", files => ["EfficientNet-B0.tflite"], task => image_classification},
        #{repo => "qualcomm/GoogLeNet", files => ["GoogLeNet.tflite"], task => image_classification},
        #{repo => "qualcomm/GoogLeNetQuantized", files => ["GoogLeNetQuantized.tflite"], task => image_classification},
        #{repo => "qualcomm/HRNetPose", files => ["HRNetPose.tflite"], task => image_classification},
        #{repo => "qualcomm/HRNetPoseQuantized", files => ["HRNetPoseQuantized.tflite"], task => image_classification},
        #{repo => "qualcomm/Inception-v3", files => ["Inception-v3.tflite"], task => image_classification},
        #{repo => "qualcomm/Inception-v3Quantized", files => ["Inception-v3Quantized.tflite"], task => image_classification},
        #{repo => "qualcomm/LiteHRNet", files => ["LiteHRNet.tflite"], task => image_classification},
        #{repo => "qualcomm/MNASNet05", files => ["MNASNet05.tflite"], task => image_classification},
        #{repo => "qualcomm/MediaPipe-Pose-Estimation", files => ["MediaPipePoseDetector.tflite", "MediaPipePoseLandmarkDetector.tflite"], task => image_classification},
        #{repo => "qualcomm/MobileNet-v2", files => ["MobileNet-v2.tflite"], task => image_classification},
        #{repo => "qualcomm/MobileNet-v2-Quantized", files => ["MobileNet-v2-Quantized.tflite"], task => image_classification},
        #{repo => "qualcomm/MobileNet-v3-Large", files => ["MobileNet-v3-Large.tflite"], task => image_classification},
        #{repo => "qualcomm/MobileNet-v3-Small", files => ["MobileNet-v3-Small.tflite"], task => image_classification},
        #{repo => "qualcomm/OpenAI-Clip", files => ["CLIPTextEncoder.tflite", "CLIPImageEncoder.tflite"], task => image_classification},
        #{repo => "qualcomm/OpenPose", files => ["OpenPose.tflite"], task => image_classification},
        #{repo => "qualcomm/RegNet", files => ["RegNet.tflite"], task => image_classification},
        #{repo => "qualcomm/ResNet18", files => ["ResNet18.tflite"], task => image_classification},
        #{repo => "qualcomm/ResNet18Quantized", files => ["ResNet18Quantized.tflite"], task => image_classification},
        #{repo => "qualcomm/ResNet50", files => ["ResNet50.tflite"], task => image_classification},
        #{repo => "qualcomm/ResNet101", files => ["ResNet101.tflite"], task => image_classification},
        #{repo => "qualcomm/ResNet101Quantized", files => ["ResNet101Quantized.tflite"], task => image_classification},
        #{repo => "qualcomm/ResNeXt50", files => ["ResNeXt50.tflite"], task => image_classification},
        #{repo => "qualcomm/ResNeXt101", files => ["ResNeXt101.tflite"], task => image_classification},
        #{repo => "qualcomm/ResNeXt101Quantized", files => ["ResNeXt101Quantized.tflite"], task => image_classification},
        #{repo => "qualcomm/Shufflenet-v2", files => ["Shufflenet-v2.tflite"], task => image_classification},
        #{repo => "qualcomm/Shufflenet-v2Quantized", files => ["Shufflenet-v2Quantized.tflite"], task => image_classification},
        #{repo => "qualcomm/SqueezeNet-1_1", files => ["SqueezeNet-1_1.tflite"], task => image_classification},
        #{repo => "qualcomm/SqueezeNet-1_1Quantized", files => ["SqueezeNet-1_1Quantized.tflite"], task => image_classification},
        #{repo => "qualcomm/Swin-Base", files => ["Swin-Base.tflite"], task => image_classification},
        #{repo => "qualcomm/Swin-Small", files => ["Swin-Small.tflite"], task => image_classification},
        #{repo => "qualcomm/Swin-Tiny", files => ["Swin-Tiny.tflite"], task => image_classification},
        #{repo => "qualcomm/VIT", files => ["VIT.tflite"], task => image_classification},
        #{repo => "qualcomm/WideResNet50", files => ["WideResNet50.tflite"], task => image_classification},
        #{repo => "qualcomm/WideResNet50-Quantized", files => ["WideResNet50-Quantized.tflite"], task => image_classification},
        
        #{repo => "qualcomm/AOT-GAN", files => ["AOT-GAN.tflite"], task => image_to_image},
        #{repo => "qualcomm/ESRGAN", files => ["ESRGAN.tflite"], task => image_to_image},
        #{repo => "qualcomm/LaMa-Dilated", files => ["LaMa-Dilated.tflite"], task => image_to_image},
        #{repo => "qualcomm/Real-ESRGAN-General-x4v3", files => ["Real-ESRGAN-General-x4v3.tflite"], task => image_to_image},
        #{repo => "qualcomm/Real-ESRGAN-x4plus", files => ["Real-ESRGAN-x4plus.tflite"], task => image_to_image},
        #{repo => "qualcomm/SESR-M5", files => ["SESR-M5.tflite"], task => image_to_image},
        #{repo => "qualcomm/SESR-M5-Quantized", files => ["SESR-M5-Quantized.tflite"], task => image_to_image},
        #{repo => "qualcomm/QuickSRNetLarge", files => ["QuickSRNetLarge.tflite"], task => image_to_image},
        #{repo => "qualcomm/QuickSRNetLarge-Quantized", files => ["QuickSRNetLarge-Quantized.tflite"], task => image_to_image},
        #{repo => "qualcomm/QuickSRNetMedium", files => ["QuickSRNetMedium.tflite"], task => image_to_image},
        #{repo => "qualcomm/QuickSRNetMedium-Quantized", files => ["QuickSRNetMedium-Quantized.tflite"], task => image_to_image},
        #{repo => "qualcomm/QuickSRNetSmall", files => ["QuickSRNetSmall.tflite"], task => image_to_image},
        #{repo => "qualcomm/QuickSRNetSmall-Quantized", files => ["QuickSRNetSmall-Quantized.tflite"], task => image_to_image},
        #{repo => "qualcomm/XLSR", files => ["XLSR.tflite"], task => image_to_image},
        #{repo => "qualcomm/XLSR-Quantized", files => ["XLSR-Quantized.tflite"], task => image_to_image},

        #{repo => "qualcomm/DeepLabV3-ResNet50", files => ["DeepLabV3-ResNet50.tflite"], task => image_segmentation},
        #{repo => "qualcomm/DDRNet23-Slim", files => ["DDRNet23-Slim.tflite"], task => image_segmentation},
        #{repo => "qualcomm/FastSam-S", files => ["FastSam-S.tflite"], task => image_segmentation},
        #{repo => "qualcomm/FastSam-X", files => ["FastSam-X.tflite"], task => image_segmentation},
        #{repo => "qualcomm/FCN_ResNet50", files => ["FCN_ResNet50.tflite"], task => image_segmentation},
        #{repo => "qualcomm/FFNet-40S", files => ["FFNet-40S.tflite"], task => image_segmentation},
        #{repo => "qualcomm/FFNet-40S-Quantized", files => ["FFNet-40S-Quantized.tflite"], task => image_segmentation},
        #{repo => "qualcomm/FFNet-54S", files => ["FFNet-54S.tflite"], task => image_segmentation},
        #{repo => "qualcomm/FFNet-54S-Quantized", files => ["FFNet-54S-Quantized.tflite"], task => image_segmentation},
        #{repo => "qualcomm/FFNet-78S", files => ["FFNet-78S.tflite"], task => image_segmentation},
        #{repo => "qualcomm/FFNet-78S-LowRes", files => ["FFNet-78S-LowRes.tflite"], task => image_segmentation},
        #{repo => "qualcomm/FFNet-78S-Quantized", files => ["FFNet-78S-Quantized.tflite"], task => image_segmentation},
        #{repo => "qualcomm/FFNet-122NS-LowRes", files => ["FFNet-122NS-LowRes.tflite"], task => image_segmentation},
        #{repo => "qualcomm/MediaPipe-Selfie-Segmentation", files => ["MediaPipe-Selfie-Segmentation.tflite"], task => image_segmentation},
        #{repo => "qualcomm/Segment-Anything-Model", files => ["SAMDecoder.tflite"], task => image_segmentation},
        #{repo => "qualcomm/SINet", files => ["SINet.tflite"], task => image_segmentation},
        #{repo => "qualcomm/Unet-Segmentation", files => ["Unet-Segmentation.tflite"], task => image_segmentation},
        #{repo => "qualcomm/Yolo-v7", files => ["Yolo-v7.tflite"], task => image_segmentation},
        #{repo => "qualcomm/Yolo-v8-Segmentation", files => ["Yolo-v8-Segmentation.tflite"], task => image_segmentation},
        #{repo => "qualcomm/YOLOv8-Segmentation", files => ["Yolo-v8-Segmentation.tflite"], task => image_segmentation},
    
        #{repo => "qualcomm/HuggingFace-WavLM-Base-Plus", files => ["HuggingFace-WavLM-Base-Plus.tflite"], task => audio_speech_recognition},
        #{repo => "qualcomm/Whisper-Small-En", files => ["WhisperDecoder.tflite", "WhisperEncoder.tflite"], task => audio_speech_recognition},
        #{repo => "qualcomm/Whisper-Base-En", files => ["WhisperDecoder.tflite", "WhisperEncoder.tflite"], task => audio_speech_recognition},
        #{repo => "qualcomm/Whisper-Tiny-En", files => ["WhisperDecoder.tflite", "WhisperEncoder.tflite"], task => audio_speech_recognition},
        
        #{repo => "qualcomm/DETR-ResNet50", files => ["DETR-ResNet50.tflite"], task => object_detection},
        #{repo => "qualcomm/DETR-ResNet50-DC5", files => ["DETR-ResNet50-DC5.tflite"], task => object_detection},
        #{repo => "qualcomm/DETR-ResNet101", files => ["DETR-ResNet101.tflite"], task => object_detection},
        #{repo => "qualcomm/DETR-ResNet101-DC5", files => ["DETR-ResNet101-DC5.tflite"], task => object_detection},
        #{repo => "qualcomm/MediaPipe-Face-Detection", files => ["MediaPipeFaceDetector.tflite", "MediaPipeFaceLandmarkDetector.tflite"], task => object_detection},
        #{repo => "qualcomm/MediaPipe-Hand-Detection", files => ["MediaPipeHandDetector.tflite", "MediaPipeHandLandmarkDetector.tflite"], task => object_detection},
        #{repo => "qualcomm/Yolo-v6", files => ["Yolo-v6.tflite"], task => object_detection},
        #{repo => "qualcomm/Yolo-v8-Detection", files => ["Yolo-v8-Detection.tflite"], task => object_detection},
        #{repo => "qualcomm/YOLOv8-Detection", files => ["Yolo-v8-Detection.tflite"], task => object_detection},

        #{repo => "qualcomm/Facebook-Denoiser", files => ["Facebook-Denoiser.tflite"], task => audio_to_audio},
        #{repo => "qualcomm/TrOCR", files => ["TrOCRDecoder.tflite", "TrOCREncoder.tflite"], task => image_to_text},
        #{repo => "qualcomm/StyleGAN2", files => ["StyleGAN2.tflite"], task => unconditional_image_generation}
    ].
