
typedef struct Node {
    int value;
    struct Node *next;
}Node;

typedef struct SRSW_Queue {
    Node *head, *tail;
}SRSW_Queue;

SRSW_Queue *srsw_queue_new();
void srsw_queue_enqueue(SRSW_Queue *q, void *value);
int srsw_queue_dequeue(SRSW_Queue *q, void **value);
int srsw_queue_get_count(SRSW_Queue *q);

