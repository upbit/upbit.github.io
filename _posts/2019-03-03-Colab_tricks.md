---
layout: post
title: Google Colab中一些你可能不知道的tricks
description: "Tricks you may not known in Google Colab"
category: tensorflow
comments: true
share: true
---

> 几年后重新开始写技术blog。一直把colab当备忘录，直到最近有人问起来BERT的各种问题，而我自己又有点记不清了(看完的代码过个年就忘干净...捂脸)
> 既然用了colab这么久，第一篇就帮colab打打广告吧 :stuck_out_tongue_closed_eyes:

# [Google Colab](https://colab.research.google.com)

Colab是一个Google提供的免费Jupyter笔记本环境，完全保存于云端，并且有免费的GPU/TPU可以使用：

https://colab.research.google.com/notebooks/welcome.ipynb

## 1. 用代码清空单元格输出 `clear_output()`

在BERT教程里看到的方法。有时想看迭代进度，但最后又不想滑动很久看eval的效果输出，就可以在train完成后加上`clear_output()`

```py
from IPython.display import clear_output

# 这里输出很多内容，比如训练modle
clear_output() # 执行后之前的输出将被清空
```


## 2. 访问 Google Drive 上的数据

colab的数据不是永久存储的，一些训练好的model或者私有数据，往往需要存储到gdrive上：

```py
from google.colab import drive
drive.mount('/content/gdrive')
GDRIVE_ROOT = "/content/gdrive/My Drive"
!ls "$GDRIVE_ROOT/datasets"
```

执行后会输出个认证url和输入框，点击url后复制里面的key，粘贴在输入框里回车即可。

之后的`!ls`语法，感叹号和jupyter里一样，代表执行shell，python的变量可以类似上面的`$GDRIVE_ROOT`来访问


## 3. Eager Execution

这个估计大部分同学都知道。colab类似anaconda默认安装了很多基础库。当然如果缺少，还是可以通过`!pip`来安装和升级的

```py
import tensorflow as tf

tf.enable_eager_execution()
```


## 4. Cell的标题和参数控制

注释后加`@title`且放在顶行，右侧会出现对应的标题内容。而某个变量赋值后，加上`@param`则可以提供输入选项：

```py
#@title 演示如何提供参数控制

LOG_DIR = '/tmp/model' #@param {type:"string"}
text_and_dropdown = 'faces' #@param ["faces", "celebs", "cats", "cars", "bedrooms", "anime"]
seed = 123456 #@param {type:"slider", min: 0, max: 10000000, step: 1}
```


## 5. Tensorboard

使用ngork可以开启tensorboard。如果logdir变更了，重新执行这段代码获取新地址即可：

```py
# @title Tensorboard

!ps aux | grep tensorboard | awk '{print $2}' | xargs kill -9
!ps aux | grep ngrok | awk '{print $2}' | xargs kill -9

!test -f ngrok-stable-linux-amd64.zip || wget https://bin.equinox.io/c/4VmDzA7iaHb/ngrok-stable-linux-amd64.zip
!test -f ngrok || unzip ngrok-stable-linux-amd64.zip

LOG_DIR = '/tmp/model-wide_n_deep' #@param {type:"string"}
get_ipython().system_raw(
    'tensorboard --logdir {} --host 0.0.0.0 --port 6006 &'
    .format(LOG_DIR)
)

get_ipython().system_raw('./ngrok http 6006 &')
! curl -s http://localhost:4040/api/tunnels | python3 -c \
    "import sys, json; print(json.load(sys.stdin)['tunnels'][0]['public_url'])"
```


## 6. 修改某个pypi库的源码

有些情况下不想使用pip的版本，但又不想自己重新打包whl，可以试下`pip --editable`这种安装方式。

例如下面这段，每次都安装`pytorch-pretrained-BERT`的master代码，然后就可以尽情修改或者覆盖代码文件了

```py
import sys

!test -d pytorch-pretrained-BERT || git clone https://github.com/huggingface/pytorch-pretrained-BERT.git pytorch-pretrained-BERT
if not 'pytorch-pretrained-BERT' in sys.path:
    sys.path += ['pytorch-pretrained-BERT']

!cd pytorch-pretrained-BERT && pip install -q --editable .
```

